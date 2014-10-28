/**********************************************************************************************\
* Rapture JSON Library                                                                         *
* Version 1.0.6                                                                                *
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

trait Extractors {

  type JsonExtractor[T] = Extractor[T, JsonDataType[_, _ <: JsonAst]]
  
  implicit def jsonExtractor[JsonType <: JsonDataType[JsonType, _ <: JsonAst]]
      (implicit ast: JsonAst): Extractor[Json, JsonType] =
    BasicExtractor({ x =>
      Json.construct(VCell(jsonSerializer.serialize(x)), Vector())
    })

  implicit def jsonBufferExtractor[JsonType <: JsonDataType[JsonType, _ <: JsonAst]]
      (implicit ast: JsonBufferAst): Extractor[JsonBuffer, JsonType] =
    BasicExtractor({ x =>
      JsonBuffer.construct(VCell(jsonSerializer.serialize(x)), Vector())
    })

  case class JsonCastExtractor[T](ast: JsonAst)

  implicit val stringExtractor: JsonExtractor[String] =
    BasicExtractor(x => x.$ast.getString(x.$root.value))

  implicit val doubleExtractor: JsonExtractor[Double] =
    BasicExtractor(x => x.$ast.getDouble(x.$root.value))

  implicit val booleanExtractor: JsonExtractor[Boolean] =
    BasicExtractor(x => x.$ast.getBoolean(x.$root.value))
  
  implicit val bigDecimalExtractor: JsonExtractor[BigDecimal] =
    BasicExtractor(x => x.$ast.getBigDecimal(x.$root.value))
  
  implicit val bigIntExtractor: JsonExtractor[BigInt] =
    BasicExtractor(x => x.$ast.getBigDecimal(x.$root.value).toBigInt)
}


