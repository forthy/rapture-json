/**********************************************************************************************\
* Rapture JSON Library                                                                         *
* Version 0.9.0                                                                                *
*                                                                                              *
* The primary distribution site is                                                             *
*                                                                                              *
*   http://rapture.io/                                                                         *
*                                                                                              *
* Copyright 2010-2013 Jon Pretty, Propensive Ltd.                                              *
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

import language.higherKinds
import language.experimental.macros

trait MacroImplicits {
  
  implicit def extractorMacro[T <: Product]: Extractor[T] =
    macro Macros.extractorMacro[T]
  
  implicit def jsonizerMacro[T <: Product]: Jsonizer[T] =
    macro Macros.jsonizerMacro[T]
  
}

object `package` extends MacroImplicits {

  implicit val intJsonizer: Jsonizer[Int] =
    new Jsonizer[Int] { def jsonize(i: Int) = i.toDouble }
  
  implicit val booleanJsonizer: Jsonizer[Boolean] =
    new Jsonizer[Boolean] { def jsonize(b: Boolean) = b }
  
  implicit val stringJsonizer: Jsonizer[String] =
    new Jsonizer[String] { def jsonize(s: String) = s }
  
  implicit val floatJsonizer: Jsonizer[Float] =
    new Jsonizer[Float] { def jsonize(f: Float) = f }
  
  implicit val doubleJsonizer: Jsonizer[Double] =
    new Jsonizer[Double] { def jsonize(d: Double) = d }
  
  implicit val longJsonizer: Jsonizer[Long] =
    new Jsonizer[Long] { def jsonize(l: Long) = l.toDouble }
  
  implicit val shortJsonizer: Jsonizer[Short] =
    new Jsonizer[Short] { def jsonize(s: Short) = s.toDouble }
  
  implicit val byteJsonizer: Jsonizer[Byte] =
    new Jsonizer[Byte] { def jsonize(b: Byte) = b.toDouble }
  
  implicit def listJsonizer[T: Jsonizer]: Jsonizer[List[T]] =
    new Jsonizer[List[T]] { def jsonize(xs: List[T]) = xs.map(implicitly[Jsonizer[T]].jsonize) }
  
  implicit def genSeqJsonizer[T: Jsonizer]: Jsonizer[Traversable[T]] =
    new Jsonizer[Traversable[T]] {
      def jsonize(xs: Traversable[T]): List[Any] =
        xs.map(implicitly[Jsonizer[T]].jsonize _).to[List]
    }
  
  implicit def mapJsonizer[T: Jsonizer]: Jsonizer[Map[String, T]] =
    new Jsonizer[Map[String, T]] {
      def jsonize(m: Map[String, T]) = m.mapValues(implicitly[Jsonizer[T]].jsonize)
    }
  
  implicit def jsonStrings(sc: StringContext)(implicit parser: JsonParser[String]) =
    new JsonStrings(sc)

  /** Companion object for Extractor type. Defines very simple extractor methods for different
    * types which may be contained within. */
  implicit val noopExtractor = BasicExtractor[Json](identity)
  implicit def noopExtractor2(implicit parser: JsonBufferParser[String]) =
    BasicExtractor[JsonBuffer](x => JsonBuffer.parse(x.toString)(parser,
        strategy.throwExceptions))
  
  implicit val stringExtractor = BasicExtractor[String](x =>
      x.parser.getString(x.json))
  
  implicit val doubleExtractor = BasicExtractor[Double](x =>
      x.parser.getDouble(x.json))
  
  implicit val floatExtractor = BasicExtractor[Float](x =>
      x.parser.getDouble(x.json).toFloat)

  implicit val shortExtractor = BasicExtractor[Short](x =>
      x.parser.getDouble(x.json).toShort)

  implicit val intExtractor = BasicExtractor[Int](x =>
      x.parser.getDouble(x.json).toInt)

  implicit val longExtractor = BasicExtractor[Long](x =>
      x.parser.getDouble(x.json).toLong)

  implicit val byteExtractor = BasicExtractor[Byte](x =>
      x.parser.getDouble(x.json).toInt.toByte)

  implicit val booleanExtractor = BasicExtractor[Boolean](x =>
      x.parser.getBoolean(x.json))

  implicit val anyExtractor = BasicExtractor[Any](_.json)
  
  def listExtractor[T: Extractor]: Extractor[List[T]] =
    BasicExtractor[List[T]](x =>
      x.parser.getArray(x).to[List] map { y =>
        implicitly[Extractor[T]].rawConstruct(y, x.parser)
      }
    )
 
  implicit def genSeqExtractor[T, Coll[_]](implicit cbf:
      scala.collection.generic.CanBuildFrom[Nothing, T, Coll[T]], ext: Extractor[T]):
      Extractor[Coll[T]] =
    BasicExtractor[Coll[T]]({ x =>
      listExtractor[T](ext).construct(x).to[Coll]
    })

  implicit def optionExtractor[T](implicit ext: Extractor[T]): Extractor[Option[T]] =
    new BasicExtractor[Option[T]](x =>
      if(x.json == null) None else Some(x.json: Any) map (ext.rawConstruct(_, x.parser))
    ) { override def errorToNull = true }
  
  implicit def mapExtractor[T](implicit ext: Extractor[T]): Extractor[Map[String, T]] =
    BasicExtractor[Map[String, T]](x =>
      x.parser.getObject(x.json) mapValues { y =>
        ext.rawConstruct(y, x.parser)
      }
    )
}

