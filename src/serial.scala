/**********************************************************************************************\
* Rapture JSON Library                                                                         *
* Version 1.0.0                                                                                *
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

import scala.reflect.macros._
import scala.annotation._

import language.experimental.macros
import language.higherKinds

trait Serializers {

  implicit def identitySerializer(implicit ast: JsonAst): Serializer[Json, Json] =
    new Serializer[Json, Json] { def serialize(j: Json) = j.$root.value }

  implicit def intSerializer(implicit ast: JsonAst): Serializer[Int, Json] =
    new Serializer[Int, Json] { def serialize(i: Int) = ast.fromDouble(i.toDouble) }

  implicit def booleanSerializer(implicit ast: JsonAst): Serializer[Boolean, Json] =
    new Serializer[Boolean, Json] { def serialize(b: Boolean) = ast.fromBoolean(b) }

  implicit def stringSerializer(implicit ast: JsonAst): Serializer[String, Json] =
    new Serializer[String, Json] { def serialize(s: String) = ast.fromString(s) }

  implicit def floatSerializer(implicit ast: JsonAst): Serializer[Float, Json] =
    new Serializer[Float, Json] { def serialize(f: Float) = ast.fromDouble(f.toDouble) }

  implicit def doubleSerializer(implicit ast: JsonAst): Serializer[Double, Json] =
    new Serializer[Double, Json] { def serialize(d: Double) = ast.fromDouble(d) }

  implicit def longSerializer(implicit ast: JsonAst): Serializer[Long, Json] =
    new Serializer[Long, Json] { def serialize(l: Long) = ast.fromDouble(l.toDouble) }

  implicit def shortSerializer(implicit ast: JsonAst): Serializer[Short, Json] =
    new Serializer[Short, Json] { def serialize(s: Short) = ast.fromDouble(s.toDouble) }

  implicit def byteSerializer(implicit ast: JsonAst): Serializer[Byte, Json] =
    new Serializer[Byte, Json] { def serialize(b: Byte) = ast.fromDouble(b.toDouble) }

  implicit def listSerializer[T](implicit ast: JsonAst, ser: Serializer[T, Json]):
      Serializer[List[T], Json] = new Serializer[List[T], Json] {
    def serialize(xs: List[T]) = ast.fromArray(xs.map(ser.serialize)) }

  implicit def genSeqSerializer[T](implicit ast: JsonAst, ser: Serializer[T, Json]):
      Serializer[Traversable[T], Json] = new Serializer[Traversable[T], Json] {
      def serialize(xs: Traversable[T]) = ast.fromArray(xs.map(ser.serialize).to[List])
    }

  implicit def mapSerializer[T](implicit ast: JsonAst, ser: Serializer[T, Json]):
      Serializer[Map[String, T], Json] = new Serializer[Map[String, T], Json] {
      def serialize(m: Map[String, T]) = ast.fromObject(m.mapValues(ser.serialize))
    }
 
  implicit def jsonSerializer(implicit ast: JsonBufferAst): Serializer[Json, JsonBuffer] =
    new Serializer[Json, JsonBuffer] { def serialize(j: Json) = j.$root.value }

  implicit def jsonBufferSerializer(implicit ast: JsonAst): Serializer[JsonBuffer, Json] =
    new Serializer[JsonBuffer, Json] { def serialize(j: JsonBuffer) = j.$root.value }

  implicit def identitySerializer2(implicit ast: JsonBufferAst): Serializer[JsonBuffer, JsonBuffer] =
    new Serializer[JsonBuffer, JsonBuffer] { def serialize(j: JsonBuffer) = j.$root.value }

  implicit def intSerializer2(implicit ast: JsonBufferAst): Serializer[Int, JsonBuffer] =
    new Serializer[Int, JsonBuffer] { def serialize(i: Int) = ast.fromDouble(i.toDouble) }

  implicit def booleanSerializer2(implicit ast: JsonBufferAst): Serializer[Boolean, JsonBuffer] =
    new Serializer[Boolean, JsonBuffer] { def serialize(b: Boolean) = ast.fromBoolean(b) }

  implicit def stringSerializer2(implicit ast: JsonBufferAst): Serializer[String, JsonBuffer] =
    new Serializer[String, JsonBuffer] { def serialize(s: String) = ast.fromString(s) }

  implicit def floatSerializer2(implicit ast: JsonBufferAst): Serializer[Float, JsonBuffer] =
    new Serializer[Float, JsonBuffer] { def serialize(f: Float) = ast.fromDouble(f.toDouble) }

  implicit def doubleSerializer2(implicit ast: JsonBufferAst): Serializer[Double, JsonBuffer] =
    new Serializer[Double, JsonBuffer] { def serialize(d: Double) = ast.fromDouble(d) }

  implicit def longSerializer2(implicit ast: JsonBufferAst): Serializer[Long, JsonBuffer] =
    new Serializer[Long, JsonBuffer] { def serialize(l: Long) = ast.fromDouble(l.toDouble) }

  implicit def shortSerializer2(implicit ast: JsonBufferAst): Serializer[Short, JsonBuffer] =
    new Serializer[Short, JsonBuffer] { def serialize(s: Short) = ast.fromDouble(s.toDouble) }

  implicit def byteSerializer2(implicit ast: JsonBufferAst): Serializer[Byte, JsonBuffer] =
    new Serializer[Byte, JsonBuffer] { def serialize(b: Byte) = ast.fromDouble(b.toDouble) }

  implicit def listSerializer2[T](implicit ast: JsonBufferAst, ser: Serializer[T, JsonBuffer]):
      Serializer[List[T], JsonBuffer] = new Serializer[List[T], JsonBuffer] {
    def serialize(xs: List[T]) = ast.fromArray(xs.map(ser.serialize)) }

  implicit def genSeqSerializer2[T](implicit ast: JsonBufferAst, ser: Serializer[T, JsonBuffer]):
      Serializer[Traversable[T], JsonBuffer] = new Serializer[Traversable[T], JsonBuffer] {
      def serialize(xs: Traversable[T]) = ast.fromArray(xs.map(ser.serialize).to[List])
    }

  implicit def mapSerializer2[T](implicit ast: JsonBufferAst, ser: Serializer[T, JsonBuffer]):
      Serializer[Map[String, T], JsonBuffer] = new Serializer[Map[String, T], JsonBuffer] {
      def serialize(m: Map[String, T]) = ast.fromObject(m.mapValues(ser.serialize))
    }
}

class LowPriorityFormatters {
  /** Formats the JSON object for multi-line readability. */
  protected def generalFormatter[Ast <: JsonAst](json: Any, ln: Int, ast: Ast, pad: String = " ",
      brk: String = "\n"): String = {
    val indent = pad*ln
    json match {
      case j =>
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
            s"${indent}${pad}${generalFormatter(v, ln + 1, ast, pad, brk)}"
          } mkString s",${brk}", s"${indent}]") mkString brk
        } else if(ast.isObject(j)) {
          val keys = ast.getKeys(j)
          if(keys.isEmpty) "{}" else List("{", keys map { k =>
            val inner = ast.dereferenceObject(j, k)
            s"""${indent}${pad}"${k}":${pad}${generalFormatter(inner, ln + 1, ast, pad, brk)}"""
          } mkString s",${brk}", s"${indent}}") mkString brk
        } else if(ast.isNull(j)) "null"
        else if(j == DataCompanion.Empty) "empty"
        else "undefined"
    }
  }
  
  implicit def humanReadable[Ast <: JsonAst](implicit ast: Ast): Formatter[Ast] { type Out = String } = new Formatter[Ast] {
    type Out = String
    def format(json: Any): String = generalFormatter(json, 0, ast, " ", "\n")  
  }

}

object formatters extends LowPriorityFormatters {
  implicit def compact[Ast <: JsonAst](implicit ast: Ast): Formatter[Ast] { type Out = String } = new Formatter[Ast] {
    type Out = String
    def format(json: Any): String = generalFormatter(json, 0, ast, "", "")
  }
}

