/**********************************************************************************************\
* Rapture JSON Library                                                                         *
* Version 0.9.0                                                                                *
*                                                                                              *
* The primary distribution site is                                                             *
*                                                                                              *
*   http://rapture.io/                                                                         *
*                                                                                              *
* Copyright 2010-2014 Jon Pretty, Propensive Ltd.                                              *
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
import language.higherKinds

object DataCompanion {
  object Empty
}

trait DataCompanion[+Type <: DataType[Type, ParserType], ParserType[S] <: DataParser[S]] {

  def empty(implicit parser: ParserType[_]) =
    construct(parser.fromObject(Map()), Vector())

  def construct(any: Any, path: Vector[Either[Int, String]])(implicit parser: ParserType[_]): Type = constructRaw(Array(any), path)
  
  def constructRaw(any: Array[Any], path: Vector[Either[Int, String]])(implicit parser: ParserType[_]): Type
  
  def parse[Source: ParserType](s: Source)(implicit eh: ExceptionHandler):
      eh.![Type, ParseException] = eh.wrap {
    construct(try implicitly[ParserType[Source]].parse(s).get catch {
      case e: NoSuchElementException => throw new ParseException(s.toString)
    }, Vector())
  }
  
  def apply[T: Jsonizer](t: T)(implicit parser: ParserType[_]): Type =
    construct(implicitly[Jsonizer[T]].jsonize(t), Vector())
  
  def unapply(value: Any)(implicit parser: ParserType[_]): Option[Type] =
    Some(construct(value, Vector()))

  def format(value: Option[Any], ln: Int, parser: ParserType[_], pad: String,
      brk: String): String
  
}

trait JsonDataCompanion[+Type <: JsonDataType[Type, ParserType],
    ParserType[S] <: JsonParser[S]] extends DataCompanion[Type, ParserType] {

  /** Formats the JSON object for multi-line readability. */
  def format(json: Option[Any], ln: Int, parser: ParserType[_], pad: String = " ",
      brk: String = "\n"): String = {
    val indent = " "*ln
    json match {
      case None => "null"
      case Some(j) =>
        if(parser.isString(j)) {
          "\""+parser.getString(j).replaceAll("\\\\", "\\\\\\\\").replaceAll("\r",
              "\\\\r").replaceAll("\n", "\\\\n").replaceAll("\"", "\\\\\"")+"\""
        } else if(parser.isBoolean(j)) {
          if(parser.getBoolean(j)) "true" else "false"
        } else if(parser.isNumber(j)) {
          val n = parser.getDouble(j)
          if(n == n.floor) n.toInt.toString else n.toString
        } else if(parser.isArray(j)) {
          List("[", parser.getArray(j) map { v =>
            s"${indent}${pad}${format(Some(v), ln + 1, parser)}"
          } mkString s",${brk}", s"${indent}]") mkString brk
        } else if(parser.isObject(j)) {
          List("{", parser.getKeys(j) map { k =>
            val inner = try Some(parser.dereferenceObject(j, k)) catch {
              case e: Exception => None
            }
            s"""${indent}${pad}"${k}":${pad}${format(inner, ln + 1, parser)}"""
          } mkString s",${brk}", s"${indent}}") mkString brk
        } else if(parser.isNull(j)) "null"
        else if(j == DataCompanion.Empty) "empty"
        else "undefined"
    }
  }
}

trait DataType[+T <: DataType[T, ParserType], ParserType[S] <: DataParser[S]] extends Dynamic {
  def companion: DataCompanion[T, ParserType]
  protected def root: Array[Any]
  implicit def parser: ParserType[_]
  def path: Vector[Either[Int, String]]
  protected def doNormalize(orEmpty: Boolean): Any
  def normalize = doNormalize(false)
  
  /** Navigates the JSON using the `List[String]` parameter, and returns the element at that
    * position in the tree. */
  def normalizeOrNil: Any =
    try normalize catch { case e: Exception => parser.fromArray(List()) }

  def normalizeOrEmpty: Any =
    try normalize catch { case e: Exception => parser.fromObject(Map()) }

  def format: String = companion.format(Some(normalize), 0, parser, " ", "\n")

  def serialize: String = companion.format(Some(normalize), 0, parser, "", "")
  
  def apply(i: Int): T =
    companion.constructRaw(root, Left(i) +: path)
  
  def applyDynamic(key: String)(i: Int): T = selectDynamic(key).apply(i)
 
  private type SomeJsonDataType = JsonDataType[_, P] forSome { type P[_] } 

  override def equals(any: Any) = any match {
    case any: SomeJsonDataType => root(0) == any.root(0)
    case _ => false
  }

  def as[T](implicit ext: Extractor[T], eh: ExceptionHandler): eh.![T, DataGetException]
  
  override def hashCode = root(0).hashCode & "json".hashCode

  /** Assumes the Json object wraps a `Map`, and extracts the element `key`. */
  def selectDynamic(key: String): T =
    companion.constructRaw(root, Right(key) +: path)

  def extract(sp: Vector[String]): DataType[T, ParserType] =
    if(sp.isEmpty) this else selectDynamic(sp.head).extract(sp.tail)
  
  override def toString = format
  
}

trait MutableDataType[+T <: DataType[T, ParserType], ParserType[S] <: MutableDataParser[S]]
    extends DataType[T, ParserType] {

  def setRoot(value: Any): Unit

  def updateParents(p: Vector[Either[Int, String]], newVal: Any): Unit = p match {
    case Vector() =>
      setRoot(newVal)
    case Left(idx) +: init =>
      val jb = companion.constructRaw(root, init)
      updateParents(init, parser.setArrayValue(jb.normalizeOrNil, idx, newVal))
    case Right(key) +: init =>
      val jb = companion.constructRaw(root, init)
      updateParents(init, parser.setObjectValue(jb.normalizeOrEmpty, key, newVal))
  }
  
  /** Updates the element `key` of the JSON object with the value `v` */
  def updateDynamic(key: String)(v: ForcedConversion): Unit =
    updateParents(path, parser.setObjectValue(normalizeOrEmpty, key, v.value))
 
  /** Updates the `i`th element of the JSON array with the value `v` */
  def update[T: Jsonizer](i: Int, v: T): Unit =
    updateParents(path, parser.setArrayValue(normalizeOrNil, i,
        implicitly[Jsonizer[T]].jsonize(v)))

  /** Removes the specified key from the JSON object */
  def -=(k: String): Unit = updateParents(path, parser.removeObjectValue(doNormalize(true), k))

  /** Adds the specified value to the JSON array */
  def +=[T: Jsonizer](v: T): Unit = {
    val r = doNormalize(true)
    val insert = if(r == DataCompanion.Empty) parser.fromArray(Nil) else r
    updateParents(path, parser.addArrayValue(insert, implicitly[Jsonizer[T]].jsonize(v)))
  }
  
}

trait JsonDataType[+T <: JsonDataType[T, ParserType], ParserType[S] <: JsonParser[S]]
    extends DataType[T, ParserType] {
  
  /** Assumes the Json object is wrapping a `T`, and casts (intelligently) to that type. */
  def as[T](implicit ext: Extractor[T], eh: ExceptionHandler): eh.![T, DataGetException] =
    eh wrap {
      try ext.construct(new Json(Array(normalize))(parser)) catch {
        case TypeMismatchException(f, e, _) => throw TypeMismatchException(f, e, path)
        case e: MissingValueException => throw e
      }
    }
  
  protected def doNormalize(orEmpty: Boolean): Any =
    yCombinator[(Any, Vector[Either[Int, String]]), Any] { fn => _ match {
      case (j, Vector()) => j: Any
      case (j, t :+ Right(k)) =>
        fn(({
          if(parser.isObject(j)) {
            try parser.dereferenceObject(j, k) catch {
              case TypeMismatchException(f, e, _) =>
                TypeMismatchException(f, e, path.drop(t.length))
              case e: Exception =>
                if(orEmpty) DataCompanion.Empty
                else throw MissingValueException(path.drop(t.length))
            }
          } else {
            throw TypeMismatchException(parser.getType(j), JsonTypes.Object,
              path.drop(t.length))
          }
        }, t))
      case (j, t :+ Left(i)) =>
        fn((
          if(parser.isArray(j)) {
            try parser.dereferenceArray(j, i) catch {
              case TypeMismatchException(f, e, _) =>
                TypeMismatchException(f, e, path.drop(t.length))
              case e: Exception =>
                if(orEmpty) DataCompanion.Empty
                else throw MissingValueException(path.drop(t.length))
            }
          } else {
            throw TypeMismatchException(parser.getType(j), JsonTypes.Array, path.drop(t.length))
          }
        , t))
    } } (root(0) -> path)
}

/** Companion object to the `Json` type, providing factory and extractor methods, and a JSON
  * pretty printer. */
object Json extends JsonDataCompanion[Json, JsonParser] {
  def constructRaw(any: Array[Any], path: Vector[Either[Int, String]])(implicit parser: JsonParser[_]): Json =
    new Json(any, path)
  
  def convert(json: Json)(implicit parser: JsonParser[_]): Json = {
    val oldParser = json.parser
    
    def convert(j: Any): Any =
      if(oldParser.isString(j)) parser.fromString(oldParser.getString(j))
      else if(oldParser.isBoolean(j)) parser.fromBoolean(oldParser.getBoolean(j))
      else if(oldParser.isNumber(j)) parser.fromDouble(oldParser.getDouble(j))
      else if(oldParser.isArray(j)) parser.fromArray(oldParser.getArray(j).map(convert))
      else if(oldParser.isObject(j)) parser.fromObject(oldParser.getObject(j).mapValues(convert))
      else null

    new Json(Array(convert(json.root(0))), json.path)(parser)
  }

}

/** Represents some parsed JSON. */
class Json(val root: Array[Any], val path: Vector[Either[Int, String]] = Vector())(implicit
    val parser: JsonParser[_]) extends JsonDataType[Json, JsonParser] {

  val companion = Json
  def $accessInnerJsonMap(k: String): Any = parser.dereferenceObject(root(0), k)

}

object JsonBuffer extends JsonDataCompanion[JsonBuffer, JsonBufferParser] {
  def constructRaw(any: Array[Any], path: Vector[Either[Int, String]])(implicit parser:
      JsonBufferParser[_]): JsonBuffer = new JsonBuffer(any, path)
  
}

class JsonBuffer(protected val root: Array[Any], val path: Vector[Either[Int, String]] = Vector())(implicit val parser: JsonBufferParser[_]) extends JsonDataType[JsonBuffer, JsonBufferParser] with MutableDataType[JsonBuffer, JsonBufferParser] {
 
  val companion = JsonBuffer
  
  def setRoot(value: Any) = root(0) = value
}

class AnyExtractor[T](cast: Json => T) extends BasicExtractor[T](x => cast(x))

case class BasicExtractor[T](val cast: Json => T) extends Extractor[T] {
  def construct(js: Json) = cast(js)
}

case class CascadeExtractor[T](casts: (Json => T)*) extends Extractor[T] {
  def construct(js: Json) = {
    (casts.foldLeft(None: Option[T]) { case (v, next) =>
      v orElse { try Some(next(js)) catch { case e: Exception => None } }
    }).get
  }
}

object ForcedConversion {
  implicit def forceConversion[T: Jsonizer](t: T) =
    ForcedConversion(implicitly[Jsonizer[T]].jsonize(t))
}
case class ForcedConversion(var value: Any)

