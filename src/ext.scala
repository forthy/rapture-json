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

import scala.reflect.macros._
import scala.annotation._

import language.experimental.macros
import language.higherKinds

trait Extractors {

  implicit def identityExtractor[D]: Extractor[D, D] = BasicExtractor[D, D](identity)

  implicit val stringExtractor: Extractor[String, Json] = BasicExtractor[String, Json](x =>
      x.$ast.getString(x.$root.value))

  implicit val doubleExtractor: Extractor[Double, Json] = BasicExtractor[Double, Json](x =>
      x.$ast.getDouble(x.$root.value))

  implicit val booleanExtractor: Extractor[Boolean, Json] = BasicExtractor[Boolean, Json](x =>
      x.$ast.getBoolean(x.$root.value))

  implicit val stringExtractor2: Extractor[String, JsonBuffer] =
    BasicExtractor[String, JsonBuffer](x => x.$ast.getString(x.$root.value))

  implicit val doubleExtractor2: Extractor[Double, JsonBuffer] = BasicExtractor[Double, JsonBuffer](x =>
      x.$ast.getDouble(x.$root.value))

  implicit val booleanExtractor2: Extractor[Boolean, JsonBuffer] = BasicExtractor[Boolean, JsonBuffer](x =>
      x.$ast.getBoolean(x.$root.value))
}


