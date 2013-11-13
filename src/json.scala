/**********************************************************************************************\
* Rapture JSON Library                                                                         *
* Version 0.9.0                                                                                *
*                                                                                              *
* The primary distribution site is                                                             *
*                                                                                              *
*   http://rapture.io/                                                                         *
*                                                                                              *
* Copyright 2010-2013 Propensive Ltd.                                                          *
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
package rapture
import rapture.core._
import rapture._

import language.dynamics
import language.experimental.macros
import scala.collection.mutable.{ListBuffer, HashMap}

trait MacroImplicits {
  implicit def materialize[T <: Product]: Extractor[T] = macro CaseClassExtraction.materialize[T]
}

/** Some useful JSON shortcuts */
object json extends MacroImplicits {

  /** Represents a JSON parser implementation which is used throughout this library */
  trait JsonParser {
    def parse(s: String): Option[Any]
    def parseMutable(s: String): Option[Any] = try Some(yCombinator[Any, Any] { fn =>
      _ match {
        case m: Map[_, _] =>
          val hm = HashMap[String, Any](m.asInstanceOf[Map[String, Any]].to[List]: _*)
          for(k <- hm.keys) hm(k) = fn(hm(k))
          hm
        case lst: List[_] => ListBuffer(lst.map(fn): _*)
        case x => x
      }
    } (parse(s).get)) catch { case e: Exception => None }
  }
  
  /** The default JSON parser implementation */
  implicit val ScalaJsonParser = new JsonParser {
    
    import scala.util.parsing.json._
    
    def parse(s: String): Option[Any] = JSON.parseFull(s)
  }

  /** Provides support for JSON literals, in the form json" { } " or json""" { } """. Interpolation
    * is used to substitute variable names into the JSON, and to extract values from a JSON string.
    */
  @inline implicit class JsonStrings(sc: StringContext)(implicit jp: JsonParser) extends {
    object json {
      /** Creates a new interpolated JSON object. */
      def apply(exprs: Any*)(implicit eh: ExceptionHandler): eh.![ParseException, Json] =
        eh.except {
          val sb = new StringBuilder
          val textParts = sc.parts.iterator
          val expressions = exprs.iterator
          sb.append(textParts.next())
          while(textParts.hasNext) {
            sb.append(new Json(expressions.next).toString)
            sb.append(textParts.next)
          }
          Json.parse(sb.toString)(jp, strategy.throwExceptions)
        }

      /** Extracts values in the structure specified from parsed JSON.  Each element in the JSON
        * structure is compared with the JSON to extract from.  Broadly speaking, elements whose
        * values are specified in the extractor must match, whereas variable elements appearing
        * in the extractor must exist. Lists may not appear in the extractor. */
      def unapplySeq(json: Json): Option[Seq[Json]] = try {
        val placeholder = Utils.uniqueNonSubstring(sc.parts.mkString)
        val PlaceholderNumber = (placeholder+"([0-9]+)"+placeholder).r
        val next = new Counter(0)
        val txt = sc.parts.reduceLeft(_ + s""""${placeholder}${next()}${placeholder}" """ + _)
        val paths: Array[Vector[String]] = Array.fill[Vector[String]](sc.parts.length - 1)(Vector())
        
        def extract(struct: Any, path: Vector[String]): Unit = {
          struct match {
            case d: Double =>
              if(json.extract(path).get[Double](doubleExtractor,
                  strategy.throwExceptions) != d) throw new Exception("Value doesn't match")
            case s: String =>
              if(json.extract(path).get[String](stringExtractor,
                  strategy.throwExceptions) != s) throw new Exception("Value doesn't match")
            case m: Map[_, _] => m foreach {
              case (k, v) => v match {
                case PlaceholderNumber(n) if v.isInstanceOf[String] =>
                  paths(n.toInt) = path :+ k.asInstanceOf[String]
                case _ => extract(v, path :+ k.asInstanceOf[String])
              }
            }
            case a: List[_] => ()
              // Emit an exception if attempting to extract on lists
          }
        }
        extract(jp.parse(txt).get, Vector())
        
        val extracts = paths.map(json.extract)
        if(extracts.exists(_.json == null)) None
        else Some(extracts map { x => new Json(x.normalize) })
      } catch { case e: Exception => None }
    }
  }

  object JsonBuffer {
    def parse(s: String)(implicit jp: JsonParser, eh: ExceptionHandler):
        eh.![ParseException, JsonBuffer] = eh.except {
      new JsonBuffer(try jp.parseMutable(s).get catch {
        case e: NoSuchElementException => throw new ParseException(s)
      })
    }
  }

  /** Companion object to the `Json` type, providing factory and extractor methods, and a JSON
    * pretty printer. */
  object Json {

    /** Parses a string containing JSON into a `Json` object */
    def parse(s: String)(implicit jp: JsonParser, eh: ExceptionHandler):
        eh.![ParseException, Json] = eh.except {
      new Json(try jp.parse(s).get catch {
        case e: NoSuchElementException => throw new ParseException(s)
      })
    }

    /** Wraps a map into a JSON object */
    def apply(map: Map[String, Any]): Json = new Json(map)

    /** Wraps a list into a JSON array */
    def apply(list: List[Any]): Json = new Json(list)

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
          "\""+s.replaceAll("\\\\", "\\\\\\\\").replaceAll("\r", "\\\\r").replaceAll("\n", "\\\\n").
              replaceAll("\"", "\\\\\"")+"\""
        case Some(n: Int) => n.toString
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

  /** Companion object for Extractor type. Defines very simple extractor methods for different
    * types which may be contained within. */
  implicit val noopExtractor = BasicExtractor[Json](x => new Json(x))
  implicit val noopExtractor2 = BasicExtractor[JsonBuffer](x => new JsonBuffer(x))
  implicit val stringExtractor = BasicExtractor[String](_.asInstanceOf[String])
  implicit val doubleExtractor = BasicExtractor[Double](_.asInstanceOf[Double])
  implicit val intExtractor = BasicExtractor[Int]({ x => try x.asInstanceOf[Int] catch {
      case e: ClassCastException => x.asInstanceOf[Double].toInt } })
  implicit val byteExtractor = BasicExtractor[Byte]({ x => try x.asInstanceOf[Int].toByte catch {
      case e: ClassCastException => x.asInstanceOf[Double].toByte } })
  implicit val longExtractor = BasicExtractor[Long](_.asInstanceOf[Double].toLong)
  implicit val booleanExtractor = BasicExtractor[Boolean](_.asInstanceOf[Boolean])
  implicit val anyExtractor = BasicExtractor[Any](identity)
  
  implicit def listExtractor[T: Extractor]: Extractor[List[T]] =
    BasicExtractor[List[T]](_.asInstanceOf[Seq[Any]].to[List] map { x =>
      implicitly[Extractor[T]].construct(x)
    })
  
  implicit def optionExtractor[T](implicit ext: Extractor[T]): Extractor[Option[T]] =
    new BasicExtractor[Option[T]](x => if(x == null) None else Some(x.asInstanceOf[Any]).map(
        ext.construct)) {
      override def errorToNull = true
    }
  
  implicit def mapExtractor[T](implicit ext: Extractor[T]): Extractor[Map[String, T]] =
    BasicExtractor[Map[String, T]](_.asInstanceOf[scala.collection.Map[String, Any]].
        toMap.mapValues(ext.construct))

  case class BasicExtractor[T](val cast: Any => T) extends Extractor[T] {
    def construct(any: Any) = cast(any)
  }

  class JsonExtractor[T](cast: Json => T) extends BasicExtractor[T](x => cast(new Json(x)))

  case class CascadeExtractor[T](casts: (Json => T)*) extends Extractor[T] {
    def construct(any: Any) = {
      val json = new Json(any)
      (casts.foldLeft(None: Option[T]) { case (v, next) =>
        v orElse { try Some(next(json)) catch { case e: Exception => None } }
      }).get
    }
  }

  class Json(private[json] val json: Any, path: Vector[Either[Int, String]] = Vector())
      extends Dynamic {

    /** Assumes the Json object is wrapping a List, and extracts the `i`th element from the vector */
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
            case e: ClassCastException => throw MissingValueException()
            case e: IndexOutOfBoundsException => throw MissingValueException()
          }, t)
        case (j, t :+ Right(k)) =>
          fn(try j.asInstanceOf[Map[String, Any]](k) catch {
            case e: ClassCastException => throw MissingValueException()
            case e: NoSuchElementException => throw MissingValueException()
          }, t)
          
      } } (json -> path)
    }

    /** Assumes the Json object is wrapping a `T`, and casts (intelligently) to that type. */
    def get[T](implicit extractor: Extractor[T], eh: ExceptionHandler):
        eh.![JsonGetException, T] = eh.except(try extractor.construct(if(extractor.errorToNull)
            (try normalize catch { case e: Exception => null }) else normalize) catch {
          case e: MissingValueException => throw e
          case e: Exception => throw new TypeMismatchException()
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
    def update(i: Int, v: Any): Unit = normalize(true, true).asInstanceOf[ListBuffer[Any]](i) = v

    /** Removes the specified key from the JSON object */
    def -=(k: String): Unit = normalize(false, true).asInstanceOf[HashMap[String, Any]].remove(k)

    /** Adds the specified value to the JSON array */
    def +=(v: Any): Unit = normalize(true, true).asInstanceOf[ListBuffer[Any]] += v

    /** Assumes the Json object is wrapping a ListBuffer, and extracts the `i`th element from the
      * list */
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
            case e: ClassCastException => throw MissingValueException()
            case e: IndexOutOfBoundsException => throw MissingValueException()
          }, t)
        case (j, Right(k) +: t) =>
          val obj = if(array && t.isEmpty) new ListBuffer[Any] else new HashMap[String, Any]()
          fn(try {
            if(modify) j.asInstanceOf[HashMap[String, Any]].getOrElseUpdate(k, obj)
            else j.asInstanceOf[HashMap[String, Any]](k)
          } catch {
            case e: ClassCastException => throw MissingValueException()
            case e: NoSuchElementException => throw MissingValueException()
          }, t)
          
      } } (json -> path.reverse)
    }

    /** Assumes the Json object is wrapping a `T`, and casts (intelligently) to that type. */
    def get[T](implicit extractor: Extractor[T], eh: ExceptionHandler):
        eh.![JsonGetException, T] =
          eh.except(try extractor.construct(if(extractor.errorToNull)
              (try normalize(false, false) catch { case e: Exception => null }) else
              normalize(false, false)) catch {
            case e: MissingValueException => throw e
            case e: Exception => throw new TypeMismatchException()
          })

    /** Assumes the Json object is wrapping a List, and returns an iterator over the list */
    def iterator: Iterator[JsonBuffer] =
      normalize(true, false).asInstanceOf[ListBuffer[JsonBuffer]].iterator

    override def toString =
      try Json.format(Some(normalize(false, false)), 0) catch {
        case e: JsonGetException => "<error>"
      }
  }
  
  sealed trait JsonGetException extends RuntimeException
  case class TypeMismatchException() extends JsonGetException
  case class MissingValueException() extends JsonGetException
}
