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

  implicit def identitySerializer(implicit ast: JsonAst): Serializer[Json] =
    new Serializer[Json] { def serialize(j: Json) = j.$root.value }

  implicit def intSerializer(implicit ast: JsonAst): Serializer[Int] =
    new Serializer[Int] { def serialize(i: Int) = ast.fromDouble(i.toDouble) }

  implicit def booleanSerializer(implicit ast: JsonAst): Serializer[Boolean] =
    new Serializer[Boolean] { def serialize(b: Boolean) = ast.fromBoolean(b) }

  implicit def stringSerializer(implicit ast: JsonAst): Serializer[String] =
    new Serializer[String] { def serialize(s: String) = ast.fromString(s) }

  implicit def floatSerializer(implicit ast: JsonAst): Serializer[Float] =
    new Serializer[Float] { def serialize(f: Float) = ast.fromDouble(f.toDouble) }

  implicit def doubleSerializer(implicit ast: JsonAst): Serializer[Double] =
    new Serializer[Double] { def serialize(d: Double) = ast.fromDouble(d) }

  implicit def longSerializer(implicit ast: JsonAst): Serializer[Long] =
    new Serializer[Long] { def serialize(l: Long) = ast.fromDouble(l.toDouble) }

  implicit def shortSerializer(implicit ast: JsonAst): Serializer[Short] =
    new Serializer[Short] { def serialize(s: Short) = ast.fromDouble(s.toDouble) }

  implicit def byteSerializer(implicit ast: JsonAst): Serializer[Byte] =
    new Serializer[Byte] { def serialize(b: Byte) = ast.fromDouble(b.toDouble) }

  implicit def listSerializer[T: Serializer](implicit ast: JsonAst): Serializer[List[T]] =
    new Serializer[List[T]] { def serialize(xs: List[T]) = ast.fromArray(xs.map(?[Serializer[T]].serialize)) }

  implicit def genSeqSerializer[T: Serializer](implicit ast: JsonAst): Serializer[Traversable[T]] =
    new Serializer[Traversable[T]] {
      def serialize(xs: Traversable[T]) =
        ast.fromArray(xs.map(?[Serializer[T]].serialize).to[List])
    }

  implicit def mapSerializer[T: Serializer](implicit ast: JsonAst): Serializer[Map[String, T]] =
    new Serializer[Map[String, T]] {
      def serialize(m: Map[String, T]) = ast.fromObject(m.mapValues(?[Serializer[T]].serialize))
    }
}

