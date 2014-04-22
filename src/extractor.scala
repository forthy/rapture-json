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
trait Extractor[T] {
  def construct(any: Json): T
  //def rawConstruct(any: Any, parser: JsonParser[_]): T = construct(new Json(Array(any))(parser))
  def errorToNull = false
}

object Extractor {

  implicit val jsonExtractor: Extractor[Json] = BasicExtractor[Json](identity)

  implicit val stringExtractor: Extractor[String] = BasicExtractor[String](x =>
      x.parser.getString(x.root(0)))

  implicit val doubleExtractor: Extractor[Double] = BasicExtractor[Double](x =>
      x.parser.getDouble(x.root(0)))

  implicit val floatExtractor: Extractor[Float] = BasicExtractor[Float](x =>
      x.parser.getDouble(x.root(0)).toFloat)

  implicit val shortExtractor: Extractor[Short] = BasicExtractor[Short](x =>
      x.parser.getDouble(x.root(0)).toShort)

  implicit val intExtractor: Extractor[Int] = BasicExtractor[Int](x =>
      x.parser.getDouble(x.root(0)).toInt)

  implicit val longExtractor: Extractor[Long] = BasicExtractor[Long](x =>
      x.parser.getDouble(x.root(0)).toLong)

  implicit val byteExtractor: Extractor[Byte] = BasicExtractor[Byte](x =>
      x.parser.getDouble(x.root(0)).toInt.toByte)

  implicit val booleanExtractor: Extractor[Boolean] = BasicExtractor[Boolean](x =>
      x.parser.getBoolean(x.root(0)))

  implicit val anyExtractor: Extractor[Any] = BasicExtractor[Any](_.root(0))

  implicit def genSeqExtractor[T, Coll[_]](implicit cbf:
      scala.collection.generic.CanBuildFrom[Nothing, T, Coll[T]], ext: Extractor[T]):
      Extractor[Coll[T]] =
    BasicExtractor[Coll[T]]({ x =>
      x.parser.getArray(x.root(0)).to[List].map(v => ext.construct(new Json(Array(v))(x.parser))).to[Coll]
    })

  implicit def optionExtractor[T](implicit ext: Extractor[T]): Extractor[Option[T]] =
    new BasicExtractor[Option[T]](x =>
      if(x.root(0) == null) None else Some(x.root(0): Any) map (v => ext.construct(new Json(Array(v))(x.parser)))
    ) { override def errorToNull = true }

  implicit def mapExtractor[T](implicit ext: Extractor[T]): Extractor[Map[String, T]] =
    BasicExtractor[Map[String, T]](x =>
      x.parser.getObject(x.root(0)) mapValues (v => ext.construct(new Json(Array(v))(x.parser)))
    )
}
