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
import rapture.data._

import scala.collection.mutable.{ListBuffer, HashMap}

import language.dynamics
import language.higherKinds

trait JsonDataCompanion[+Type <: JsonDataType[Type, RepresentationType],
    RepresentationType <: JsonRepresentation] extends DataCompanion[Type, RepresentationType] {

  /** Formats the JSON object for multi-line readability. */
  def format(json: Option[Any], ln: Int, representation: RepresentationType, pad: String = " ",
      brk: String = "\n"): String = {
    val indent = pad*ln
    json match {
      case None => "null"
      case Some(j) =>
        if(representation.isString(j)) {
          "\""+representation.getString(j).replaceAll("\\\\", "\\\\\\\\").replaceAll("\r",
              "\\\\r").replaceAll("\n", "\\\\n").replaceAll("\"", "\\\\\"")+"\""
        } else if(representation.isBoolean(j)) {
          if(representation.getBoolean(j)) "true" else "false"
        } else if(representation.isNumber(j)) {
          val n = representation.getDouble(j)
          if(n == n.floor) n.toInt.toString else n.toString
        } else if(representation.isArray(j)) {
          List("[", representation.getArray(j) map { v =>
            s"${indent}${pad}${format(Some(v), ln + 1, representation, pad, brk)}"
          } mkString s",${brk}", s"${indent}]") mkString brk
        } else if(representation.isObject(j)) {
          List("{", representation.getKeys(j) map { k =>
            val inner = try Some(representation.dereferenceObject(j, k)) catch {
              case e: Exception => None
            }
            s"""${indent}${pad}"${k}":${pad}${format(inner, ln + 1, representation, pad, brk)}"""
          } mkString s",${brk}", s"${indent}}") mkString brk
        } else if(representation.isNull(j)) "null"
        else if(j == DataCompanion.Empty) "empty"
        else "undefined"
    }
  }
}

trait JsonDataType[+T <: JsonDataType[T, RepresentationType], RepresentationType <: JsonRepresentation]
    extends DataType[T, RepresentationType] {
 
  def wrap(any: Any): T

  /** Assumes the Json object is wrapping a `T`, and casts (intelligently) to that type. */
  def as[S](implicit ext: Extractor[S, T], eh: ExceptionHandler): eh.![S, DataGetException] =
    eh wrap {
      try ext.construct(wrap(normalize)) catch {
        case TypeMismatchException(f, e, _) => throw TypeMismatchException(f, e, path)
        case e: MissingValueException => throw e
      }
    }
 
  protected def doNormalize(orEmpty: Boolean): Any =
    yCombinator[(Any, Vector[Either[Int, String]]), Any] { fn => _ match {
      case (j, Vector()) => j: Any
      case (j, t :+ Right(k)) =>
        fn(({
          if(representation.isObject(j)) {
            try representation.dereferenceObject(j, k) catch {
              case TypeMismatchException(f, e, _) =>
                TypeMismatchException(f, e, path.drop(t.length))
              case e: Exception =>
                if(orEmpty) DataCompanion.Empty
                else throw MissingValueException(path.drop(t.length))
            }
          } else {
            throw TypeMismatchException(representation.getType(j), DataTypes.Object,
              path.drop(t.length))
          }
        }, t))
      case (j, t :+ Left(i)) =>
        fn((
          if(representation.isArray(j)) {
            try representation.dereferenceArray(j, i) catch {
              case TypeMismatchException(f, e, _) =>
                TypeMismatchException(f, e, path.drop(t.length))
              case e: Exception =>
                if(orEmpty) DataCompanion.Empty
                else throw MissingValueException(path.drop(t.length))
            }
          } else {
            throw TypeMismatchException(representation.getType(j), DataTypes.Array, path.drop(t.length))
          }
        , t))
    } } (root(0) -> path)
}

/** Companion object to the `Json` type, providing factory and extractor methods, and a JSON
  * pretty printer. */
object Json extends JsonDataCompanion[Json, JsonRepresentation] {
  def constructRaw(any: Array[Any], path: Vector[Either[Int, String]])(implicit representation: JsonRepresentation): Json =
    new Json(any, path)
  
  def convert(json: Json)(implicit representation: JsonRepresentation): Json = {
    val oldRepresentation = json.representation
    
    def convert(j: Any): Any =
      if(oldRepresentation.isString(j))
        representation.fromString(oldRepresentation.getString(j))
      else if(oldRepresentation.isBoolean(j))
        representation.fromBoolean(oldRepresentation.getBoolean(j))
      else if(oldRepresentation.isNumber(j))
        representation.fromDouble(oldRepresentation.getDouble(j))
      else if(oldRepresentation.isArray(j))
        representation.fromArray(oldRepresentation.getArray(j).map(convert))
      else if(oldRepresentation.isObject(j))
        representation.fromObject(oldRepresentation.getObject(j).mapValues(convert))
      else representation.nullValue

    new Json(Array(convert(json.root(0))), json.path)(representation)
  }

}

/** Represents some parsed JSON. */
class Json(val root: Array[Any], val path: Vector[Either[Int, String]] = Vector())(implicit
    val representation: JsonRepresentation) extends JsonDataType[Json, JsonRepresentation] {

  def wrap(any: Any): Json = new Json(Array(any))
  val companion = Json
  def $accessInnerMap(k: String): Any = representation.dereferenceObject(root(0), k)

}

object JsonBuffer extends JsonDataCompanion[JsonBuffer, JsonBufferRepresentation] {
  def constructRaw(any: Array[Any], path: Vector[Either[Int, String]])(implicit representation:
      JsonBufferRepresentation): JsonBuffer = new JsonBuffer(any, path)
  
}

class JsonBuffer(protected val root: Array[Any], val path: Vector[Either[Int, String]] = Vector())(implicit val representation: JsonBufferRepresentation) extends JsonDataType[JsonBuffer, JsonBufferRepresentation] with MutableDataType[JsonBuffer, JsonBufferRepresentation] {
 
  def wrap(any: Any): JsonBuffer = new JsonBuffer(Array(any))
  val companion = JsonBuffer
  def setRoot(value: Any) = root(0) = value
}

//class AnyExtractor[T](cast: Json => T) extends BasicExtractor[T](x => cast(x))

/*case class CascadeExtractor[T](casts: (Json => T)*) extends Extractor[T] {
  def construct(js: Json) = {
    (casts.foldLeft(None: Option[T]) { case (v, next) =>
      v orElse { try Some(next(js)) catch { case e: Exception => None } }
    }).get
  }
}*/

