/**********************************************************************************************\
* Rapture JSON Library                                                                         *
* Version 1.0.5                                                                                *
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

object JsonMacros {
  def jsonExtractorMacro[T: c.WeakTypeTag](c: Context): c.Expr[Extractor[T, Json]] =
    Macros.extractorMacro[T, Json](c)
  
  def jsonBufferExtractorMacro[T: c.WeakTypeTag](c: Context): c.Expr[Extractor[T, JsonBuffer]] =
    Macros.extractorMacro[T, JsonBuffer](c)
  
  def jsonSerializerMacro[T: c.WeakTypeTag](c: Context)(ast: c.Expr[JsonAst]): c.Expr[Serializer[T, Json]] =
    Macros.serializerMacro[T, Json](c)(ast)
  
  def jsonBufferSerializerMacro[T: c.WeakTypeTag](c: Context)(ast: c.Expr[JsonBufferAst]):
      c.Expr[Serializer[T, JsonBuffer]] = Macros.serializerMacro[T, JsonBuffer](c)(ast)
}

