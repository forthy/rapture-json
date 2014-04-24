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
  implicit def floatExtractor[J](implicit ext: Extractor[Double, J]): Extractor[Float, J] =
    ext.map(_.toFloat)

  implicit def shortExtractor[J](implicit ext: Extractor[Double, J]): Extractor[Short, J] =
    ext.map(_.toShort)

  implicit def intExtractor[J](implicit ext: Extractor[Double, J]): Extractor[Int, J] =
    ext.map(_.toInt)

  implicit def longExtractor[J](implicit ext: Extractor[Double, J]): Extractor[Long, J] =
    ext.map(_.toLong)

  implicit def byteExtractor[J](implicit ext: Extractor[Double, J]): Extractor[Byte, J] =
    ext.map(_.toInt.toByte)

  implicit def anyExtractor[J <: DataType[_, DataRepresentation]]: Extractor[Any, J] =
    BasicExtractor[Any, J](_.root(0))

  
  implicit def genSeqExtractor[T, Coll[_], R <: DataRepresentation, J <: DataType[J, R]](implicit cbf:
      scala.collection.generic.CanBuildFrom[Nothing, T, Coll[T]],
      ext: Extractor[T, J]): Extractor[Coll[T], J] = BasicExtractor[Coll[T], J]({ x =>
      x.representation.getArray(x.root(0)).to[List].map(v => ext.construct(x.wrap(v))).to[Coll]
      })

  implicit def optionExtractor[T, R <: DataRepresentation, J <: DataType[J, R]]
      (implicit ext: Extractor[T, J]): Extractor[Option[T], J] =
    new BasicExtractor[Option[T], J](x => try Some(x.root(0): Any) map (v =>
        ext.construct(x.wrap(v))) catch { case e: Exception => None })

  implicit def mapExtractor[T, R <: DataRepresentation, J <: DataType[J, R]]
      (implicit ext: Extractor[T, J]): Extractor[Map[String, T], J] =
    BasicExtractor[Map[String, T], J](x => x.representation.getObject(x.root(0)) mapValues {
        v => ext.construct(x.wrap(v)) })
}

@implicitNotFound("cannot extract type ${T} from JSON.")
trait Extractor[T, -D] { ext =>
  def construct(any: D): T
  def map[T2](fn: T => T2) = new Extractor[T2, D] {
    def construct(any: D) = fn(ext.construct(any))
  }
}

case class BasicExtractor[T, -D](val cast: D => T) extends Extractor[T, D] {
  def construct(d: D) = cast(d)
}
