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

import scala.collection.mutable.{ListBuffer, HashMap}
import scala.collection.JavaConverters

package jsonParsers {
  package scalaJson {
    object `package` {
      implicit val scalaJsonParser = ScalaJsonParser
    }
  }
}

object JsonTypes {
  sealed class JsonType(val name: String)
  case object Number extends JsonType("number")
  case object String extends JsonType("string")
  case object Null extends JsonType("null")
  case object Boolean extends JsonType("boolean")
  case object Array extends JsonType("array")
  case object Object extends JsonType("object")
  case object Undefined extends JsonType("undefined")
}

/** Represents a JSON parser implementation which is used throughout this library */
trait JsonParser[Source] {
  def parse(s: Source): Option[Any]

  /** Extracts a `Boolean` from the parsed JSON. */
  def getBoolean(boolean: Any): Boolean

  def fromBoolean(boolean: Boolean): Any

  /** Extracts a `String` from the parsed JSON. */
  def getString(string: Any): String

  def fromString(string: String): Any

  /** Extracts a `Double` from the parsed JSON. */
  def getDouble(number: Any): Double

  def fromDouble(number: Double): Any

  /** Extracts a JSON object as a `Map[String, Any]` from the parsed JSON. */
  def getObject(obj: Any): Map[String, Any]

  def fromObject(obj: Map[String, Any]): Any

  /** Extracts a JSON array as a `Seq[Any]` from the parsed JSON. */
  def getArray(array: Any): Seq[Any]

  def fromArray(array: Seq[Any]): Any

  /** Dereferences the named element within the JSON object. */
  def dereferenceObject(obj: Any, element: String): Any =
    getObject(obj)(element)
  
  /** Returns at `Iterator[String]` over the names of the elements in the JSON object. */
  def getKeys(obj: Any): Iterator[String] =
    getObject(obj).keys.iterator
 
  /** Gets the indexed element from the parsed JSON array. */
  def dereferenceArray(array: Any, element: Int): Any =
    getArray(array)(element)

  /** Tests if the element represents a `Boolean` */
  def isBoolean(any: Any): Boolean
  
  /** Tests if the element represents a `String` */
  def isString(any: Any): Boolean
  
  /** Tests if the element represents a number */
  def isNumber(any: Any): Boolean
  
  /** Tests if the element represents an `Object` */
  def isObject(any: Any): Boolean
  
  /** Tests if the element represents an `Array` */
  def isArray(any: Any): Boolean
  
  /** Tests if the element represents a `null` */
  def isNull(any: Any): Boolean

  /** Returns the JsonType instance for the particular type. */
  def getType(any: Any): JsonTypes.JsonType =
    if(isBoolean(any)) JsonTypes.Boolean
    else if(isString(any)) JsonTypes.String
    else if(isNumber(any)) JsonTypes.Number
    else if(isObject(any)) JsonTypes.Object
    else if(isArray(any)) JsonTypes.Array
    else if(isNull(any)) JsonTypes.Null
    else JsonTypes.Undefined

  protected def typeTest(pf: PartialFunction[Any, Unit])(v: Any) = pf.isDefinedAt(v)
}

trait JsonBufferParser[T] extends JsonParser[T] {

  def setObjectValue(obj: Any, name: String, value: Any): Any
  def setArrayValue(array: Any, index: Int, value: Any): Any
  def removeObjectValue(obj: Any, name: String): Any
  def addArrayValue(array: Any, value: Any): Any
}

/** The default JSON parser implementation */
object ScalaJsonParser extends JsonBufferParser[String] {
  
  import scala.util.parsing.json._

  def getArray(array: Any): List[Any] = array match {
    case list: List[a] => list
    case _ => throw TypeMismatchException(getType(array), JsonTypes.Array, Vector())
  }

  def fromArray(array: Seq[Any]): Any = array.to[List]

  def getBoolean(boolean: Any): Boolean = boolean match {
    case boolean: Boolean => boolean
    case _ => throw TypeMismatchException(getType(boolean), JsonTypes.Boolean, Vector())
  }
  
  def fromBoolean(boolean: Boolean): Any = boolean
  
  def getDouble(double: Any): Double = double match {
    case double: Double => double
    case _ => throw TypeMismatchException(getType(double), JsonTypes.Number, Vector())
  }
  
  def fromDouble(double: Double): Any = double
  
  def getString(string: Any): String = string match {
    case string: String => string
    case _ => throw TypeMismatchException(getType(string), JsonTypes.String, Vector())
  }
  
  def fromString(string: String): Any = string
  
  def getObject(obj: Any): Map[String, Any] = obj match {
    case obj: Map[_, _] => obj collect { case (k: String, v) => k -> v }
    case _ => throw TypeMismatchException(getType(obj), JsonTypes.Object, Vector())
  }
  
  def fromObject(obj: Map[String, Any]): Any = obj
  
  def setObjectValue(obj: Any, name: String, value: Any): Any = obj match {
    case obj: Map[_, _] => obj.asInstanceOf[Map[String, Any]] + (name -> value)
    case _ => throw TypeMismatchException(getType(obj), JsonTypes.Object, Vector())
  }
  
  def removeObjectValue(obj: Any, name: String): Any = obj match {
    case obj: Map[_, _] => obj.asInstanceOf[Map[String, Any]] - name
    case _ => throw TypeMismatchException(getType(obj), JsonTypes.Object, Vector())
  }
  
  def addArrayValue(array: Any, value: Any): Any = array match {
    case array: List[_] => array ::: List(value)
    case _ => throw TypeMismatchException(getType(array), JsonTypes.Array, Vector())
  }
  
  def setArrayValue(array: Any, index: Int, value: Any): Any = array match {
    case array: List[_] => array.patch(index, List(value), 1)
    case _ => throw TypeMismatchException(getType(array), JsonTypes.Array, Vector())
  }
  
  
  def isBoolean(any: Any): Boolean = typeTest { case _: Boolean => () } (any)
  def isString(any: Any): Boolean = typeTest { case _: String => () } (any)
  def isNumber(any: Any): Boolean = typeTest { case _: Double => () } (any)
  def isObject(any: Any): Boolean = typeTest { case _: Map[_, _] => () } (any)
  def isArray(any: Any): Boolean = typeTest { case _: List[_] => () } (any)
  def isNull(any: Any): Boolean = any == null
  
  def parse(s: String): Option[Any] = JSON.parseFull(s)
}

