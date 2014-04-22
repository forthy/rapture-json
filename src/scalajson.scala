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

package jsonParsers {
  package scalaJson {
    object `package` {
      implicit val scalaJsonRepresentation = ScalaJsonRepresentation
    }
  }
}

/** The default JSON representation implementation */
object ScalaJsonRepresentation extends JsonBufferRepresentation[String] {
  
  import scala.util.parsing.json._

  def getArray(array: Any): List[Any] = array match {
    case list: List[a] => list
    case _ => throw TypeMismatchException(getType(array), DataTypes.Array, Vector())
  }

  def fromArray(array: Seq[Any]): Any = array.to[List]

  def getBoolean(boolean: Any): Boolean = boolean match {
    case boolean: Boolean => boolean
    case _ => throw TypeMismatchException(getType(boolean), DataTypes.Boolean, Vector())
  }
  
  def fromBoolean(boolean: Boolean): Any = boolean
  
  def getDouble(double: Any): Double = double match {
    case double: Double => double
    case _ => throw TypeMismatchException(getType(double), DataTypes.Number, Vector())
  }
  
  def fromDouble(double: Double): Any = double
  
  def getString(string: Any): String = string match {
    case string: String => string
    case _ => throw TypeMismatchException(getType(string), DataTypes.String, Vector())
  }
  
  def fromString(string: String): Any = string
  
  def getObject(obj: Any): Map[String, Any] = obj match {
    case obj: Map[_, _] => obj collect { case (k: String, v) => k -> v }
    case _ => throw TypeMismatchException(getType(obj), DataTypes.Object, Vector())
  }
  
  def fromObject(obj: Map[String, Any]): Any = obj
  
  def setObjectValue(obj: Any, name: String, value: Any): Any = obj match {
    case obj: Map[_, _] => obj.asInstanceOf[Map[String, Any]] + (name -> value)
    case _ => throw TypeMismatchException(getType(obj), DataTypes.Object, Vector())
  }
  
  def removeObjectValue(obj: Any, name: String): Any = obj match {
    case obj: Map[_, _] => obj.asInstanceOf[Map[String, Any]] - name
    case _ => throw TypeMismatchException(getType(obj), DataTypes.Object, Vector())
  }
  
  def addArrayValue(array: Any, value: Any): Any = array match {
    case array: List[_] => array ::: List(value)
    case _ => throw TypeMismatchException(getType(array), DataTypes.Array, Vector())
  }
  
  def setArrayValue(array: Any, index: Int, value: Any): Any = array match {
    case array: List[_] => array.padTo(index, null).patch(index, List(value), 1)
    case _ => throw TypeMismatchException(getType(array), DataTypes.Array, Vector())
  }
  
  
  def isBoolean(any: Any): Boolean = typeTest { case _: Boolean => () } (any)
  def isString(any: Any): Boolean = typeTest { case _: String => () } (any)
  def isNumber(any: Any): Boolean = typeTest { case _: Double => () } (any)
  def isObject(any: Any): Boolean = typeTest { case _: Map[_, _] => () } (any)
  def isArray(any: Any): Boolean = typeTest { case _: List[_] => () } (any)
  def isNull(any: Any): Boolean = any == null
  def nullValue: Any = null
  
  def parse(s: String): Option[Any] = JSON.parseFull(s)
}

