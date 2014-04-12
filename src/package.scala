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

import language.higherKinds
import language.experimental.macros

object `package` {

  /*implicit def extractorMacro[T <: Product]: Unwrapper[T, Json] =
    macro Macros.extractorMacro[T]
  
  implicit def jsonizerMacro[T <: Product](implicit parser: JsonParser[_]): Wrapper[T] =
    macro Macros.jsonizerMacro[T]*/
  
  //implicit def jsonStrings(sc: StringContext)(implicit parser: JsonParser[String]) =
  //  new JsonStrings(sc)

  implicit def intWrapper(implicit parser: JsonParser[_]): Wrapper[Int] =
    new Wrapper[Int] { def wrap(i: Int) = parser.fromDouble(i.toDouble) }

  implicit def booleanWrapper(implicit parser: JsonParser[_]): Wrapper[Boolean] =
    new Wrapper[Boolean] { def wrap(b: Boolean) = parser.fromBoolean(b) }

  implicit def stringWrapper(implicit parser: JsonParser[_]): Wrapper[String] =
    new Wrapper[String] { def wrap(s: String) = parser.fromString(s) }

  implicit def floatWrapper(implicit parser: JsonParser[_]): Wrapper[Float] =
    new Wrapper[Float] { def wrap(f: Float) = parser.fromDouble(f.toDouble) }

  implicit def doubleWrapper(implicit parser: JsonParser[_]): Wrapper[Double] =
    new Wrapper[Double] { def wrap(d: Double) = parser.fromDouble(d) }

  implicit def longWrapper(implicit parser: JsonParser[_]): Wrapper[Long] =
    new Wrapper[Long] { def wrap(l: Long) = parser.fromDouble(l.toDouble) }

  implicit def shortWrapper(implicit parser: JsonParser[_]): Wrapper[Short] =
    new Wrapper[Short] { def wrap(s: Short) = parser.fromDouble(s.toDouble) }

  implicit def byteWrapper(implicit parser: JsonParser[_]): Wrapper[Byte] =
    new Wrapper[Byte] { def wrap(b: Byte) = parser.fromDouble(b.toDouble) }

  implicit def listWrapper[T: Wrapper](implicit parser: JsonParser[_]): Wrapper[List[T]] =
    new Wrapper[List[T]] { def wrap(xs: List[T]) = parser.fromArray(xs.map(implicitly[Wrapper[T]].wrap)) }

  implicit def genSeqWrapper[T: Wrapper](implicit parser: JsonParser[_]): Wrapper[Traversable[T]] =
    new Wrapper[Traversable[T]] {
      def wrap(xs: Traversable[T]) =
        parser.fromArray(xs.map(implicitly[Wrapper[T]].wrap).to[List])
    }

  implicit def mapWrapper[T: Wrapper](implicit parser: JsonParser[_]): Wrapper[Map[String, T]] =
    new Wrapper[Map[String, T]] {
      def wrap(m: Map[String, T]) = parser.fromObject(m.mapValues(implicitly[Wrapper[T]].wrap))
    }
}

