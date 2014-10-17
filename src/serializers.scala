/**********************************************************************************************\
* Rapture JSON Library                                                                         *
* Version 1.0.3                                                                                *
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

  implicit def intSerializer[Ast <: JsonAst, JsonType <: JsonDataType[JsonType, _ <: Ast]]
      (implicit ast: Ast): Serializer[Int, JsonType] =
    new Serializer[Int, JsonType] { def serialize(i: Int) = ast.fromDouble(i.toDouble) }

  implicit def booleanSerializer[Ast <: JsonAst, JsonType <: JsonDataType[JsonType, _ <: Ast]]
      (implicit ast: Ast): Serializer[Boolean, JsonType] =
    new Serializer[Boolean, JsonType] { def serialize(b: Boolean) = ast.fromBoolean(b) }

  implicit def stringSerializer[Ast <: JsonAst, JsonType <: JsonDataType[JsonType, _ <: Ast]]
      (implicit ast: Ast): Serializer[String, JsonType] =
    new Serializer[String, JsonType] { def serialize(s: String) = ast.fromString(s) }

  implicit def floatSerializer[Ast <: JsonAst, JsonType <: JsonDataType[JsonType, _ <: Ast]]
      (implicit ast: Ast): Serializer[Float, JsonType] =
    new Serializer[Float, JsonType] { def serialize(f: Float) = ast.fromDouble(f.toDouble) }

  implicit def doubleSerializer[Ast <: JsonAst, JsonType <: JsonDataType[JsonType, _ <: Ast]]
      (implicit ast: Ast): Serializer[Double, JsonType] =
    new Serializer[Double, JsonType] { def serialize(d: Double) = ast.fromDouble(d) }

  implicit def longSerializer[Ast <: JsonAst, JsonType <: JsonDataType[JsonType, _ <: Ast]]
      (implicit ast: Ast): Serializer[Long, JsonType] =
    new Serializer[Long, JsonType] { def serialize(l: Long) = ast.fromDouble(l.toDouble) }

  implicit def shortSerializer[Ast <: JsonAst, JsonType <: JsonDataType[JsonType, _ <: Ast]]
      (implicit ast: Ast): Serializer[Short, JsonType] =
    new Serializer[Short, JsonType] { def serialize(s: Short) = ast.fromDouble(s.toDouble) }

  implicit def byteSerializer[Ast <: JsonAst, JsonType <: JsonDataType[JsonType, _ <: Ast]]
      (implicit ast: Ast): Serializer[Byte, JsonType] =
    new Serializer[Byte, JsonType] { def serialize(b: Byte) = ast.fromDouble(b.toDouble) }

  implicit def traversableSerializer[Type, Coll[T] <: Traversable[T], Ast <: JsonAst,
      JsonType <: JsonDataType[JsonType, _ <: Ast]](implicit ast: Ast,
      ser: Serializer[Type, JsonType]): Serializer[Coll[Type], JsonType] =
    new Serializer[Coll[Type], JsonType] {
      def serialize(xs: Coll[Type]) = ast.fromArray(xs.map(ser.serialize).to[List])
    }

  implicit def mapSerializer[Type, Ast <: JsonAst, JsonType <: JsonDataType[JsonType, _ <: Ast]]
      (implicit ast: Ast, ser: Serializer[Type, JsonType]): Serializer[Map[String, Type],
      JsonType] =
    new Serializer[Map[String, Type], JsonType] {
      def serialize(m: Map[String, Type]) = ast.fromObject(m.mapValues(ser.serialize))
    }
 
  implicit def jsonSerializer[Ast <: JsonAst, JsonType <: JsonDataType[JsonType, _ <: JsonAst],
      JsonType2 <: JsonDataType[JsonType2, _ <: Ast]](implicit ast: Ast): Serializer[JsonType,
      JsonType2] =
    new Serializer[JsonType, JsonType2] {
      def serialize(j: JsonType) =
        if(j.$ast == ast) j.$root.value else {
          val oldAst = j.$ast

          def convert(v: Any): Any =
            if(oldAst.isString(v)) ast.fromString(oldAst.getString(v))
            else if(oldAst.isBoolean(v)) ast.fromBoolean(oldAst.getBoolean(v))
            else if(oldAst.isNumber(v)) ast.fromDouble(oldAst.getDouble(v))
            else if(oldAst.isArray(v)) ast.fromArray(oldAst.getArray(v).map(convert))
            else if(oldAst.isObject(v)) ast.fromObject(oldAst.getObject(v).mapValues(convert))
            else ast.nullValue

          convert(j.$root.value)
        }
    }
}
