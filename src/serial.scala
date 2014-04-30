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
}

