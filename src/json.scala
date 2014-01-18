/**********************************************************************************************\
* Rapture JSON Library                                                                         *
* Version 0.9.0                                                                                *
*                                                                                              *
* The primary distribution site is                                                             *
*                                                                                              *
*   http://rapture.io/                                                                         *
*                                                                                              *
* Copyright 2010-2013 Jon Pretty, Propensive Ltd.                                              *
*                                                                                              *
* Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file    *
* except in compliance with the License. You may obtain a copy of the License at               *
*                                                                                              *
*   http://www.apache.org/licenses/LICENSE-2.0                                                 *
*                                                                                              *
* Unless required by applicable law or agreed to in writing, software distributed under the    *
* License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    *
* either express or implied. See the License for the specific language governing permissions   *
* and limitations under the License.                                                           *
\**********************************************************************************************/
package rapture.json

import rapture.core._

import scala.collection.mutable.{ListBuffer, HashMap}

import language.dynamics

object JsonBuffer {
  def parse[Source: JsonParser](s: Source)(implicit eh: ExceptionHandler):
      eh.![JsonBuffer, ParseException] = eh.wrap {
    new JsonBuffer(try implicitly[JsonParser[Source]].parseMutable(s).get catch {
      case e: NoSuchElementException => throw new ParseException(s.toString)
    })
  }
}

/** Companion object to the `Json` type, providing factory and extractor methods, and a JSON
  * pretty printer. */
object Json {

  /** Parses a string containing JSON into a `Json` object */
  def parse[Source: JsonParser](s: Source)(implicit eh: ExceptionHandler):
      eh.![Json, ParseException] = eh.wrap {
    new Json(try implicitly[JsonParser[Source]].parse(s).get catch {
      case e: NoSuchElementException => throw new ParseException(s.toString)
    })
  }

  def apply[T: Jsonizer](t: T) = new Json(implicitly[Jsonizer[T]].jsonize(t))

  def wrapDynamic(any: Any) = new Json(any)

  def unapply(json: Any): Option[Json] = Some(new Json(json))

  def format(json: Json): String = format(Some(json.json), 0)
  
  /** Formats the JSON object for multi-line readability. */
  def format(json: Option[Any], ln: Int): String = {
    val indent = " "*ln
    json match {
      case Some(o: scala.collection.Map[_, _]) =>
        List("{", o.keys map { k => indent+" "+"\""+k+"\": "+format(o.get(k), ln + 1) } mkString
            ",\n", indent+"}").mkString("\n")
      case Some(a: Seq[_]) =>
        List("[", a map { v => indent+" "+format(Some(v), ln + 1) } mkString(",\n"),
            indent+"]") mkString "\n"
      case Some(s: String) =>
        "\""+s.replaceAll("\\\\", "\\\\\\\\").replaceAll("\r",
            "\\\\r").replaceAll("\n", "\\\\n").replaceAll("\"", "\\\\\"")+"\""
      case Some(n: Int) => n.toString
      case Some(n: Double) => if(n == n.floor) n.toInt.toString else n.toString
      case Some(n: Number) => n.toString
      case Some(v: Boolean) => if(v) "true" else "false"
      case Some(j: Json) => format(Some(j.json), ln)
      case Some(j: JsonBuffer) => format(Some(j.json), ln)
      case None => "null"
      case _ => "undefined"
    }
  }

  def serialize(json: Json): String = serialize(Some(json.normalize))

  def serialize(json: Option[Any]): String = {
    json match {
      case Some(o: scala.collection.Map[_, _]) =>
        List("{", o.keys map { k => "\""+k+"\":"+serialize(o.get(k)) } mkString
            ",", "}").mkString
      case Some(a: Seq[_]) =>
        List("[", a map { v => serialize(Some(v)) } mkString(","),
            "]") mkString ""
      case Some(s: String) =>
        "\""+s.replaceAll("\\\\", "\\\\\\\\").replaceAll("\n", "\\\\n").replaceAll("\"",
            "\\\\\"")+"\""
      case Some(n: Int) => n.toString
      case Some(n: Number) => n.toString
      case Some(v: Boolean) => if(v) "true" else "false"
      case Some(j: Json) => serialize(Some(j.json))
      case Some(j: JsonBuffer) => serialize(Some(j.json))
      case None => "null"
      case _ => "undefined"
    }
  }
  
}

class Json(private[json] val json: Any, path: Vector[Either[Int, String]] = Vector())
    extends Dynamic {

  override def equals(any: Any) = any match {
    case any: Json => json == any.json
    case _ => false
  }

  override def hashCode = json.hashCode & "json".hashCode

  /** Assumes the Json object is wrapping a List, and extracts the `i`th element from the
    * vector */
  def apply(i: Int): Json =
    new Json(json, Left(i) +: path)
 
  /** Combines a `selectDynamic` and an `apply`.  This is necessary due to the way dynamic
    * application is expanded. */
  def applyDynamic(key: String)(i: Int): Json = selectDynamic(key).apply(i)
  
  /** Navigates the JSON using the `Vector[String]` parameter, and returns the element at that
    * position in the tree. */
  def extract(sp: Vector[String]): Json =
    if(sp.isEmpty) this else selectDynamic(sp.head).extract(sp.tail)
  
  /** Assumes the Json object wraps a `Map`, and extracts the element `key`. */
  def selectDynamic(key: String): Json =
    new Json(json, Right(key) +: path)
 
  private[json] def normalize: Any = {
    yCombinator[(Any, Vector[Either[Int, String]]), Any] { fn => v => v match {
      case (j, Vector()) => j
      case (j, t :+ Left(i)) =>
        fn(try j.asInstanceOf[List[Any]](i) catch {
          case e: ClassCastException =>
            throw MissingValueException(path.drop(t.length))
          case e: IndexOutOfBoundsException =>
            throw MissingValueException(path.drop(t.length))
        }, t)
      case (j, t :+ Right(k)) =>
        fn(try j.asInstanceOf[Map[String, Any]](k) catch {
          case e: ClassCastException =>
            throw MissingValueException(path.drop(t.length))
          case e: NoSuchElementException =>
            throw MissingValueException(path.drop(t.length))
        }, t)
        
    } } (json -> path)
  }

  /** Assumes the Json object is wrapping a `T`, and casts (intelligently) to that type. */
  def get[T](implicit eh: ExceptionHandler, ext: Extractor[T]): eh.![T, JsonGetException] =
    eh.wrap(try ext.construct(if(ext.errorToNull)
          (try normalize catch { case e: Exception => null }) else normalize) catch {
        case e: MissingValueException => throw e
        case e: Exception => throw TypeMismatchException(path)
      })

  /** Assumes the Json object is wrapping a List, and returns an iterator over the list */
  def iterator: Iterator[Json] = json.asInstanceOf[List[Json]].iterator

  override def toString =
    try Json.format(Some(normalize), 0) catch {
      case e: JsonGetException => "<error>"
    }
}

class JsonBuffer(private[json] val json: Any, path: Vector[Either[Int, String]] = Vector())
    extends Dynamic {
  /** Updates the element `key` of the JSON object with the value `v` */
  def updateDynamic(key: String)(v: Any): Unit =
    normalize(false, true).asInstanceOf[HashMap[String, Any]](key) = v
 
  /** Updates the `i`th element of the JSON array with the value `v` */
  def update(i: Int, v: Any): Unit =
    normalize(true, true).asInstanceOf[ListBuffer[Any]](i) = v

  /** Removes the specified key from the JSON object */
  def -=(k: String): Unit =
    normalize(false, true).asInstanceOf[HashMap[String, Any]].remove(k)

  /** Adds the specified value to the JSON array */
  def +=(v: Any): Unit = normalize(true, true).asInstanceOf[ListBuffer[Any]] += v

  /** Assumes the Json object is wrapping a ListBuffer, and extracts the `i`th element from
    * the list */
  def apply(i: Int): JsonBuffer =
    new JsonBuffer(json, Left(i) +: path)
 
  /** Combines a `selectDynamic` and an `apply`.  This is necessary due to the way dynamic
    * application is expanded. */
  def applyDynamic(key: String)(i: Int): JsonBuffer = selectDynamic(key).apply(i)
  
  /** Navigates the JSON using the `List[String]` parameter, and returns the element at that
    * position in the tree. */
  def extract(sp: Vector[String]): JsonBuffer =
    if(sp.isEmpty) this else selectDynamic(sp.head).extract(sp.tail)
  
  /** Assumes the Json object wraps a `Map`, and extracts the element `key`. */
  def selectDynamic(key: String): JsonBuffer =
    new JsonBuffer(json, Right(key) +: path)
 
  private[json] def normalize(array: Boolean, modify: Boolean): Any = {
    yCombinator[(Any, Vector[Either[Int, String]]), Any] { fn => v => v match {
      case (j, Vector()) => j
      case (j, Left(i) +: t) =>
        fn(try j.asInstanceOf[ListBuffer[Any]](i) catch {
          case e: ClassCastException =>
            throw MissingValueException(path.drop(t.length))
          case e: IndexOutOfBoundsException =>
            throw MissingValueException(path.drop(t.length))
        }, t)
      case (j, Right(k) +: t) =>
        val obj = if(array && t.isEmpty) new ListBuffer[Any] else new HashMap[String, Any]()
        fn(try {
          if(modify) j.asInstanceOf[HashMap[String, Any]].getOrElseUpdate(k, obj)
          else j.asInstanceOf[HashMap[String, Any]](k)
        } catch {
          case e: ClassCastException =>
            throw MissingValueException(path.drop(t.length))
          case e: NoSuchElementException =>
            throw MissingValueException(path.drop(t.length))
        }, t)
        
    } } (json -> path.reverse)
  }

  /** Assumes the Json object is wrapping a `T`, and casts (intelligently) to that type. */
  def get[T](implicit ext: Extractor[T], eh: ExceptionHandler): eh.![T, JsonGetException] =
    eh.wrap(try ext.construct(if(ext.errorToNull)
        (try normalize(false, false) catch { case e: Exception => null }) else
        normalize(false, false)) catch {
      case e: MissingValueException => throw e
      case e: Exception => throw TypeMismatchException(path)
    })

  /** Assumes the Json object is wrapping a List, and returns an iterator over the list */
  def iterator: Iterator[JsonBuffer] =
    normalize(true, false).asInstanceOf[ListBuffer[JsonBuffer]].iterator

  override def toString =
    try Json.format(Some(normalize(false, false)), 0) catch {
      case e: JsonGetException => "<error>"
    }
}

class JsonExtractor[T](cast: Json => T) extends BasicExtractor[T](x => cast(new Json(x)))

case class BasicExtractor[T](val cast: Any => T) extends Extractor[T] {
  def construct(any: Any) = cast(any)
}

case class CascadeExtractor[T](casts: (Json => T)*) extends Extractor[T] {
  def construct(any: Any) = {
    val json = new Json(any)
    (casts.foldLeft(None: Option[T]) { case (v, next) =>
      v orElse { try Some(next(json)) catch { case e: Exception => None } }
    }).get
  }
}

object JsonGetException {
  def stringifyPath(path: Vector[Either[Int, String]]) = path.reverse map {
    case Left(i) => s"($i)"
    case Right(s) => s".$s"
  } mkString ""
}

sealed class JsonGetException(msg: String) extends RuntimeException(msg)

case class TypeMismatchException(path: Vector[Either[Int, String]])
  extends JsonGetException("Type mismatch: json"+JsonGetException.stringifyPath(path))

case class MissingValueException(path: Vector[Either[Int, String]])
  extends JsonGetException("Missing value: json"+JsonGetException.stringifyPath(path))

