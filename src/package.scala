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

import language.higherKinds
import language.experimental.macros

object `package` {

  implicit def extractorMacro[T <: Product]: Extractor[T, Json] =
    macro JsonMacros.extractorMacro[T]
  
  implicit def serializerMacro[T <: Product](implicit representation: JsonRepresentation[_]): Serializer[T] =
    macro Macros.serializerMacro[T]
  
  implicit def jsonStrings(sc: StringContext)(implicit representation: JsonRepresentation[String]) =
    new JsonStrings(sc)

}

