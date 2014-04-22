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

import scala.reflect.macros._
import scala.annotation._

import language.experimental.macros
import language.higherKinds

@implicitNotFound("Cannot serialize type ${T} to JSON. Please provide an implicit of type Serialiser[${T}].")
trait Serializer[T] { def serialize(t: T): Any }

object Serializer {

  implicit def identitySerializer(implicit parser: JsonParser[_]): Serializer[Json] =
    new Serializer[Json] { def serialize(j: Json) = j.root(0) }

  implicit def intSerializer(implicit parser: JsonParser[_]): Serializer[Int] =
    new Serializer[Int] { def serialize(i: Int) = parser.fromDouble(i.toDouble) }

  implicit def booleanSerializer(implicit parser: JsonParser[_]): Serializer[Boolean] =
    new Serializer[Boolean] { def serialize(b: Boolean) = parser.fromBoolean(b) }

  implicit def stringSerializer(implicit parser: JsonParser[_]): Serializer[String] =
    new Serializer[String] { def serialize(s: String) = parser.fromString(s) }

  implicit def floatSerializer(implicit parser: JsonParser[_]): Serializer[Float] =
    new Serializer[Float] { def serialize(f: Float) = parser.fromDouble(f.toDouble) }

  implicit def doubleSerializer(implicit parser: JsonParser[_]): Serializer[Double] =
    new Serializer[Double] { def serialize(d: Double) = parser.fromDouble(d) }

  implicit def longSerializer(implicit parser: JsonParser[_]): Serializer[Long] =
    new Serializer[Long] { def serialize(l: Long) = parser.fromDouble(l.toDouble) }

  implicit def shortSerializer(implicit parser: JsonParser[_]): Serializer[Short] =
    new Serializer[Short] { def serialize(s: Short) = parser.fromDouble(s.toDouble) }

  implicit def byteSerializer(implicit parser: JsonParser[_]): Serializer[Byte] =
    new Serializer[Byte] { def serialize(b: Byte) = parser.fromDouble(b.toDouble) }

  implicit def listSerializer[T: Serializer](implicit parser: JsonParser[_]): Serializer[List[T]] =
    new Serializer[List[T]] { def serialize(xs: List[T]) = parser.fromArray(xs.map(implicitly[Serializer[T]].serialize)) }

  implicit def genSeqSerializer[T: Serializer](implicit parser: JsonParser[_]): Serializer[Traversable[T]] =
    new Serializer[Traversable[T]] {
      def serialize(xs: Traversable[T]) =
        parser.fromArray(xs.map(implicitly[Serializer[T]].serialize).to[List])
    }

  implicit def mapSerializer[T: Serializer](implicit parser: JsonParser[_]): Serializer[Map[String, T]] =
    new Serializer[Map[String, T]] {
      def serialize(m: Map[String, T]) = parser.fromObject(m.mapValues(implicitly[Serializer[T]].serialize))
    }
}

