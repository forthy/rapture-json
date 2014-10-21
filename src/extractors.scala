/**********************************************************************************************\
* Rapture JSON Library                                                                         *
* Version 1.0.4                                                                                *
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

  implicit def jsonExtractor(implicit ast: JsonAst):
      Extractor[Json, JsonDataType[_, _ <: JsonAst]] =
    BasicExtractor(x => Json.construct(VCell(x.$root.value), x.$path))

  implicit def jsonBufferExtractor(implicit ast: JsonBufferAst):
      Extractor[JsonBuffer, JsonDataType[_, _ <: JsonAst]] =
    BasicExtractor(x => JsonBuffer.construct(VCell(x.$root.value), x.$path))

  implicit val stringExtractor: Extractor[String, JsonDataType[_, _ <: JsonAst]] =
    BasicExtractor(x => x.$ast.getString(x.$root.value))

  implicit val doubleExtractor: Extractor[Double, JsonDataType[_, _ <: JsonAst]] =
    BasicExtractor(x => x.$ast.getDouble(x.$root.value))

  implicit val booleanExtractor: Extractor[Boolean, JsonDataType[_, _ <: JsonAst]] =
    BasicExtractor(x => x.$ast.getBoolean(x.$root.value))
  
  implicit val bigDecimalExtractor: Extractor[BigDecimal, JsonDataType[_, _ <: JsonAst]] =
    BasicExtractor(x => x.$ast.getBigDecimal(x.$root.value))
  
  implicit val bigIntExtractor: Extractor[BigInt, JsonDataType[_, _ <: JsonAst]] =
    BasicExtractor(x => x.$ast.getBigDecimal(x.$root.value).toBigInt)
}


