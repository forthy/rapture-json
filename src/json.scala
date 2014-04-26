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

trait JsonDataCompanion[+Type <: JsonDataType[Type, AstType],
    AstType <: JsonAst] extends DataCompanion[Type, AstType] {

  /** Formats the JSON object for multi-line readability. */
  def format(json: Option[Any], ln: Int, ast: AstType, pad: String = " ",
      brk: String = "\n"): String = {
    val indent = pad*ln
    json match {
      case None => "null"
      case Some(j) =>
        if(ast.isString(j)) {
          "\""+ast.getString(j).replaceAll("\\\\", "\\\\\\\\").replaceAll("\r",
              "\\\\r").replaceAll("\n", "\\\\n").replaceAll("\"", "\\\\\"")+"\""
        } else if(ast.isBoolean(j)) {
          if(ast.getBoolean(j)) "true" else "false"
        } else if(ast.isNumber(j)) {
          val n = ast.getDouble(j)
          if(n == n.floor) n.toInt.toString else n.toString
        } else if(ast.isArray(j)) {
          val arr = ast.getArray(j)
          if(arr.isEmpty) "[]" else List("[", arr map { v =>
            s"${indent}${pad}${format(Some(v), ln + 1, ast, pad, brk)}"
          } mkString s",${brk}", s"${indent}]") mkString brk
        } else if(ast.isObject(j)) {
          val keys = ast.getKeys(j)
          if(keys.isEmpty) "{}" else List("{", keys map { k =>
            val inner = try Some(ast.dereferenceObject(j, k)) catch {
              case e: Exception => None
            }
            s"""${indent}${pad}"${k}":${pad}${format(inner, ln + 1, ast, pad, brk)}"""
          } mkString s",${brk}", s"${indent}}") mkString brk
        } else if(ast.isNull(j)) "null"
        else if(j == DataCompanion.Empty) "empty"
        else "undefined"
    }
  }
}

trait JsonDataType[+T <: JsonDataType[T, AstType], AstType <: JsonAst]
    extends DataType[T, AstType]

object JsonBuffer extends JsonDataCompanion[JsonBuffer, JsonBufferAst] {
  def construct(any: VCell, path: Vector[Either[Int, String]])(implicit ast:
      JsonBufferAst): JsonBuffer = new JsonBuffer(any, path)
}

/** Companion object to the `Json` type, providing factory and extractor methods, and a JSON
  * pretty printer. */
object Json extends JsonDataCompanion[Json, JsonAst] {
  def construct(any: VCell, path: Vector[Either[Int, String]])(implicit ast:
      JsonAst): Json = new Json(any, path)
  
  def convert(json: Json)(implicit ast: JsonAst): Json = {
    val oldAst = json.$ast
    
    def convert(j: Any): Any =
      if(oldAst.isString(j))
        ast.fromString(oldAst.getString(j))
      else if(oldAst.isBoolean(j))
        ast.fromBoolean(oldAst.getBoolean(j))
      else if(oldAst.isNumber(j))
        ast.fromDouble(oldAst.getDouble(j))
      else if(oldAst.isArray(j))
        ast.fromArray(oldAst.getArray(j).map(convert))
      else if(oldAst.isObject(j))
        ast.fromObject(oldAst.getObject(j).mapValues(convert))
      else ast.nullValue

    new Json(VCell(convert(json.$root.value)), json.$path)(ast)
  }
}

/** Represents some parsed JSON. */
class Json(val $root: VCell, val $path: Vector[Either[Int, String]] = Vector())(implicit
    val $ast: JsonAst) extends JsonDataType[Json, JsonAst] {
  def $wrap(any: Any, path: Vector[Either[Int, String]]): Json = new Json(VCell(any), path)
  def $deref(path: Vector[Either[Int, String]]): Json = new Json($root, path)
  def format: String = Json.format(Some($normalize), 0, $ast, " ", "\n")
  def serialize: String = Json.format(Some($normalize), 0, $ast, "", "")
  def $accessInnerMap(k: String): Any = $ast.dereferenceObject($root.value, k)
}

class JsonBuffer(val $root: VCell, val $path: Vector[Either[Int, String]] = Vector())
    (implicit val $ast: JsonBufferAst) extends
    JsonDataType[JsonBuffer, JsonBufferAst] with
    MutableDataType[JsonBuffer, JsonBufferAst] {
  def $wrap(any: Any, path: Vector[Either[Int, String]]): JsonBuffer = new JsonBuffer(VCell(any), path)
  def $deref(path: Vector[Either[Int, String]]): JsonBuffer = new JsonBuffer($root, path)
  def format: String = Json.format(Some($normalize), 0, $ast, " ", "\n")
  def serialize: String = Json.format(Some($normalize), 0, $ast, "", "")
  def $accessInnerMap(k: String): Any = $ast.dereferenceObject($root.value, k)
}
