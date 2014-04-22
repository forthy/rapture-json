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

  implicit def identitySerializer(implicit representation: JsonRepresentation[_]): Serializer[Json] =
    new Serializer[Json] { def serialize(j: Json) = j.root(0) }

  implicit def intSerializer(implicit representation: JsonRepresentation[_]): Serializer[Int] =
    new Serializer[Int] { def serialize(i: Int) = representation.fromDouble(i.toDouble) }

  implicit def booleanSerializer(implicit representation: JsonRepresentation[_]): Serializer[Boolean] =
    new Serializer[Boolean] { def serialize(b: Boolean) = representation.fromBoolean(b) }

  implicit def stringSerializer(implicit representation: JsonRepresentation[_]): Serializer[String] =
    new Serializer[String] { def serialize(s: String) = representation.fromString(s) }

  implicit def floatSerializer(implicit representation: JsonRepresentation[_]): Serializer[Float] =
    new Serializer[Float] { def serialize(f: Float) = representation.fromDouble(f.toDouble) }

  implicit def doubleSerializer(implicit representation: JsonRepresentation[_]): Serializer[Double] =
    new Serializer[Double] { def serialize(d: Double) = representation.fromDouble(d) }

  implicit def longSerializer(implicit representation: JsonRepresentation[_]): Serializer[Long] =
    new Serializer[Long] { def serialize(l: Long) = representation.fromDouble(l.toDouble) }

  implicit def shortSerializer(implicit representation: JsonRepresentation[_]): Serializer[Short] =
    new Serializer[Short] { def serialize(s: Short) = representation.fromDouble(s.toDouble) }

  implicit def byteSerializer(implicit representation: JsonRepresentation[_]): Serializer[Byte] =
    new Serializer[Byte] { def serialize(b: Byte) = representation.fromDouble(b.toDouble) }

  implicit def listSerializer[T: Serializer](implicit representation: JsonRepresentation[_]): Serializer[List[T]] =
    new Serializer[List[T]] { def serialize(xs: List[T]) = representation.fromArray(xs.map(implicitly[Serializer[T]].serialize)) }

  implicit def genSeqSerializer[T: Serializer](implicit representation: JsonRepresentation[_]): Serializer[Traversable[T]] =
    new Serializer[Traversable[T]] {
      def serialize(xs: Traversable[T]) =
        representation.fromArray(xs.map(implicitly[Serializer[T]].serialize).to[List])
    }

  implicit def mapSerializer[T: Serializer](implicit representation: JsonRepresentation[_]): Serializer[Map[String, T]] =
    new Serializer[Map[String, T]] {
      def serialize(m: Map[String, T]) = representation.fromObject(m.mapValues(implicitly[Serializer[T]].serialize))
    }
}

