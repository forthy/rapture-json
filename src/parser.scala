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

class JsonChunk {
  var depth = 0
  var content: Vector[Any] = Vector()
}

object Parser {
  def parse(s: String, start: Boolean) = {
    
    var decided = start
    var currentA = start

    var structA = new JsonChunk
    var structB = new JsonChunk

    var stringA = new StringBuilder
    var stringB = new StringBuilder

    def flip() = {

    }

    def notString() = decided = true

    var i = 0
    
    while(i < s.length) {
      s(i) match {
        case '{' =>
        case '}' =>
        case '"' => flip()
        case '\\' =>
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        case ' ' | '\t' =>
        case '\n' | '\r' => notString()
        case '[' =>
        case ']' =>
        case ',' =>
        case ':' =>
      }
      i += 1
    }
  }
}
