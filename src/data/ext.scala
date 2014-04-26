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

package rapture.data

import rapture.core._

import scala.reflect.macros._
import scala.annotation._

import language.experimental.macros
import language.higherKinds

object Extractor {
  implicit def floatExtractor[Data](implicit ext: Extractor[Double, Data]): Extractor[Float, Data] =
    ext.map(_.toFloat)

  implicit def shortExtractor[Data](implicit ext: Extractor[Double, Data]): Extractor[Short, Data] =
    ext.map(_.toShort)

  implicit def intExtractor[Data](implicit ext: Extractor[Double, Data]): Extractor[Int, Data] =
    ext.map(_.toInt)

  implicit def longExtractor[Data](implicit ext: Extractor[Double, Data]): Extractor[Long, Data] =
    ext.map(_.toLong)

  implicit def byteExtractor[Data](implicit ext: Extractor[Double, Data]): Extractor[Byte, Data] =
    ext.map(_.toInt.toByte)

  implicit def anyExtractor[Data <: DataType[_, DataRepresentation]]: Extractor[Any, Data] =
    BasicExtractor[Any, Data](_.root(0))

  
  implicit def genSeqExtractor[T, Coll[_], Data <: DataType[Data, R] forSome { type R <:
      DataRepresentation }](implicit cbf: scala.collection.generic.CanBuildFrom[Nothing, T,
      Coll[T]], ext: Extractor[T, Data]): Extractor[Coll[T], Data] =
    BasicExtractor[Coll[T], Data]({ x => println("x = "+x+";root(0) = "+x.root(0)); x.representation.getArray(x.root(0)).to[List].map({v => println("v = "+v);
        ext.construct(x.wrap(v)) }).to[Coll] })

  implicit def optionExtractor[T, Data <: DataType[Data, R] forSome { type R <: DataRepresentation }]
      (implicit ext: Extractor[T, Data]): Extractor[Option[T], Data] =
    new BasicExtractor[Option[T], Data](x => try Some(x.root(0): Any) map (v =>
        ext.construct(x.wrap(v))) catch { case e: Exception => None })

  implicit def mapExtractor[T, Data <: DataType[Data, R] forSome { type R <: DataRepresentation }]
      (implicit ext: Extractor[T, Data]): Extractor[Map[String, T], Data] =
    BasicExtractor[Map[String, T], Data](x => x.representation.getObject(x.root(0)) mapValues {
        v => ext.construct(x.wrap(v)) })
}

@implicitNotFound("cannot extract type ${T} from ${D}.")
trait Extractor[T, -D] { ext =>
  def construct(any: D): T
  def map[T2](fn: T => T2) = new Extractor[T2, D] {
    def construct(any: D): T2 = fn(ext.construct(any))
  }
}

case class BasicExtractor[T, -D](val cast: D => T) extends Extractor[T, D] {
  def construct(d: D) = cast(d)
}
