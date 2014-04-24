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

object DataGetException {
  def stringifyPath(path: Vector[Either[Int, String]]) = path.reverse map {
    case Left(i) => s"($i)"
    case Right(s) => s".$s"
  } mkString ""
}

sealed class DataGetException(msg: String) extends RuntimeException(msg)

case class TypeMismatchException(foundType: DataTypes.DataType,
    expectedType: DataTypes.DataType, path: Vector[Either[Int, String]]) extends
    DataGetException(s"Type mismatch: Expected ${expectedType.name} but found "+
    s"${foundType.name} at <value>${DataGetException.stringifyPath(path)}")

case class MissingValueException(path: Vector[Either[Int, String]])
  extends DataGetException(s"Missing value: <value>${DataGetException.stringifyPath(path)}")


