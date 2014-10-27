/**********************************************************************************************\
* Rapture JSON Library                                                                         *
* Version 1.0.5                                                                                *
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

  type JsonSerializer[T] = Serializer[T, JsonDataType[_, _ <: JsonAst]]

  case class BasicJsonSerializer[T](serialization: T => Any) extends Serializer[T,
      JsonDataType[_, _ <: JsonAst]] { def serialize(t: T): Any = serialization(t) }

  implicit def intSerializer(implicit ast: JsonAst): JsonSerializer[Int] =
    BasicJsonSerializer(ast fromDouble _.toDouble)

  implicit def booleanSerializer(implicit ast: JsonAst): JsonSerializer[Boolean] =
    BasicJsonSerializer(ast fromBoolean _)

  implicit def stringSerializer(implicit ast: JsonAst): JsonSerializer[String] =
    BasicJsonSerializer(ast fromString _)

  implicit def floatSerializer(implicit ast: JsonAst): JsonSerializer[Float] =
    BasicJsonSerializer(ast fromDouble _.toDouble)

  implicit def doubleSerializer(implicit ast: JsonAst): JsonSerializer[Double] =
    BasicJsonSerializer(ast fromDouble _)

  implicit def bigDecimalSerializer(implicit ast: JsonAst): JsonSerializer[BigDecimal] =
    BasicJsonSerializer(ast fromBigDecimal _)

  implicit def bigIntSerializer(implicit ast: JsonAst): JsonSerializer[BigInt] =
    BasicJsonSerializer(ast fromBigDecimal BigDecimal(_))

  implicit def longSerializer(implicit ast: JsonAst): JsonSerializer[Long] =
    BasicJsonSerializer(ast fromDouble _.toDouble)

  implicit def shortSerializer(implicit ast: JsonAst): JsonSerializer[Short] =
    BasicJsonSerializer(ast fromDouble _.toDouble)

  implicit def byteSerializer(implicit ast: JsonAst): JsonSerializer[Byte] =
    BasicJsonSerializer(ast fromDouble _.toDouble)

  implicit def traversableSerializer[Type: JsonSerializer, Coll[T] <: Traversable[T]]
      (implicit ast: JsonAst): JsonSerializer[Coll[Type]] =
    BasicJsonSerializer(ast fromArray _.map(?[JsonSerializer[Type]].serialize).to[List])

  implicit def mapSerializer[Type, Ast <: JsonAst, JsonType <: JsonDataType[JsonType, _ <: Ast]]
      (implicit ast: Ast, ser: Serializer[Type, JsonType]): Serializer[Map[String, Type],
      JsonType] =
    new Serializer[Map[String, Type], JsonType] {
      def serialize(m: Map[String, Type]) = ast.fromObject(m.mapValues(ser.serialize))
    }

  case class DirectJsonSerializer[T](ast: JsonAst)

  implicit def directJsonSerializer[T: DirectJsonSerializer](implicit ast: JsonAst):
      JsonSerializer[T] =
    BasicJsonSerializer[T](obj => jsonSerializer.serialize(Json.construct(VCell(obj),
        Vector())(?[DirectJsonSerializer[T]].ast)))

  implicit def jsonSerializer[JsonType <: JsonDataType[JsonType, _ <: JsonAst]]
      (implicit ast: JsonAst): JsonSerializer[JsonType] =
    BasicJsonSerializer({ j =>
      if(j.$ast == ast) j.$normalize else {
        val oldAst = j.$ast

        def convert(v: Any): Any =
          if(oldAst.isString(v)) ast.fromString(oldAst.getString(v))
          else if(oldAst.isBoolean(v)) ast.fromBoolean(oldAst.getBoolean(v))
          else if(oldAst.isNumber(v)) ast.fromDouble(oldAst.getDouble(v))
          else if(oldAst.isArray(v)) ast.fromArray(oldAst.getArray(v).map(convert))
          else if(oldAst.isObject(v)) ast.fromObject(oldAst.getObject(v).mapValues(convert))
          else ast.nullValue

        convert(j.$normalize)
      }
    })
}
