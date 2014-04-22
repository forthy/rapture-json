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

trait JsonDataCompanion[+Type <: JsonDataType[Type, ParserType],
    ParserType[S] <: JsonParser[S]] extends DataCompanion[Type, ParserType] {

  /** Formats the JSON object for multi-line readability. */
  def format(json: Option[Any], ln: Int, parser: ParserType[_], pad: String = " ",
      brk: String = "\n"): String = {
    val indent = pad*ln
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
            s"${indent}${pad}${format(Some(v), ln + 1, parser, pad, brk)}"
          } mkString s",${brk}", s"${indent}]") mkString brk
        } else if(parser.isObject(j)) {
          List("{", parser.getKeys(j) map { k =>
            val inner = try Some(parser.dereferenceObject(j, k)) catch {
              case e: Exception => None
            }
            s"""${indent}${pad}"${k}":${pad}${format(inner, ln + 1, parser, pad, brk)}"""
          } mkString s",${brk}", s"${indent}}") mkString brk
        } else if(parser.isNull(j)) "null"
        else if(j == DataCompanion.Empty) "empty"
        else "undefined"
    }
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
            throw TypeMismatchException(parser.getType(j), DataTypes.Object,
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
            throw TypeMismatchException(parser.getType(j), DataTypes.Array, path.drop(t.length))
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

