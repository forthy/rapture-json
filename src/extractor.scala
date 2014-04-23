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

@implicitNotFound("cannot extract type ${T} from JSON.")
trait Extractor[T, -D] { def construct(any: D): T }

case class BasicExtractor[T, -D](val cast: D => T) extends Extractor[T, D] {
  def construct(d: D) = cast(d)
}

object Extractor {

  implicit def identityExtractor[D]: Extractor[D, D] = BasicExtractor[D, D](identity)

  implicit val stringExtractor: Extractor[String, Json] = BasicExtractor[String, Json](x =>
      x.representation.getString(x.root(0)))

  implicit val doubleExtractor: Extractor[Double, Json] = BasicExtractor[Double, Json](x =>
      x.representation.getDouble(x.root(0)))

  implicit val floatExtractor: Extractor[Float, Json] = BasicExtractor[Float, Json](x =>
      x.representation.getDouble(x.root(0)).toFloat)

  implicit val shortExtractor: Extractor[Short, Json] = BasicExtractor[Short, Json](x =>
      x.representation.getDouble(x.root(0)).toShort)

  implicit val intExtractor: Extractor[Int, Json] = BasicExtractor[Int, Json](x =>
      x.representation.getDouble(x.root(0)).toInt)

  implicit val longExtractor: Extractor[Long, Json] = BasicExtractor[Long, Json](x =>
      x.representation.getDouble(x.root(0)).toLong)

  implicit val byteExtractor: Extractor[Byte, Json] = BasicExtractor[Byte, Json](x =>
      x.representation.getDouble(x.root(0)).toInt.toByte)

  implicit val booleanExtractor: Extractor[Boolean, Json] = BasicExtractor[Boolean, Json](x =>
      x.representation.getBoolean(x.root(0)))

  implicit val anyExtractor: Extractor[Any, Json] = BasicExtractor[Any, Json](_.root(0))

  /*implicit def genSeqExtractor[T, Coll[_]](implicit cbf:
      scala.collection.generic.CanBuildFrom[Nothing, T, Coll[T]], ext: Extractor[T]):
      Extractor[Coll[T]] =
    BasicExtractor[Coll[T]]({ x =>
      x.representation.getArray(x.root(0)).to[List].map(v => ext.construct(new Json(Array(v))(x.representation))).to[Coll]
    })

  implicit def optionExtractor[T](implicit ext: Extractor[T]): Extractor[Option[T]] =
    new BasicExtractor[Option[T]](x =>
      if(x.root(0) == null) None else Some(x.root(0): Any) map (v => ext.construct(new Json(Array(v))(x.representation)))
    )

  implicit def mapExtractor[T](implicit ext: Extractor[T]): Extractor[Map[String, T]] =
    BasicExtractor[Map[String, T]](x =>
      x.representation.getObject(x.root(0)) mapValues (v => ext.construct(new Json(Array(v))(x.representation)))
    )*/
}
