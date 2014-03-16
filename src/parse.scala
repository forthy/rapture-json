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
      implicit val scalaJson = ScalaJsonParser
    }
  }
}

/** Represents a JSON parser implementation which is used throughout this library */
trait JsonParser[Source] {
  def parse(s: Source): Option[Any]
  def parseMutable(s: Source): Option[Any] = try Some(yCombinator[Any, Any] { fn =>
    _ match {
      case m: Map[_, _] =>
        val hm = HashMap[String, Any](m.asInstanceOf[Map[String, Any]].to[List]: _*)
        for(k <- hm.keys) hm(k) = fn(hm(k))
        hm
      case lst: List[_] => ListBuffer(lst.map(fn): _*)
      case x => x
    }
  } (parse(s).get)) catch { case e: Exception => None }

  /** Extracts a `Boolean` from the parsed JSON. */
  def getBoolean(boolean: Any): Boolean

  /** Extracts a `String` from the parsed JSON. */
  def getString(string: Any): String

  /** Extracts a `Double` from the parsed JSON. */
  def getDouble(number: Any): Double

  /** Extracts a JSON object as a `Map[String, Any]` from the parsed JSON. */
  def getObject(obj: Any): Map[String, Any]

  /** Extracts a JSON array as a `Seq[Any]` from the parsed JSON. */
  def getArray(array: Any): Seq[Any]

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

  protected def typeTest(pf: PartialFunction[Any, Unit])(v: Any) = pf.isDefinedAt(v)
}

trait JsonBufferParser[T] extends JsonParser[T] {

  def isMutableArray(array: Any): Boolean
  def isMutableObject(array: Any): Boolean
  def getMutableArray(array: Any): List[Any]
  def getMutableObject(obj: Any): Map[String, Any]
  def setMutableObjectValue(obj: Any, name: String, value: Any): Unit
  def setMutableArrayValue(array: Any, index: Int, value: Any): Unit
  def removeMutableObjectValue(obj: Any, name: String): Unit
  def addMutableArrayValue(array: Any, value: Any): Unit

  def toMutable(any: Any): Any
}

/** The default JSON parser implementation */
object ScalaJsonParser extends JsonBufferParser[String] {
  
  import scala.util.parsing.json._

  def getArray(array: Any): List[Any] = array match {
    case list: List[a] => list
    case _ => throw TypeMismatchException(Vector())
  }

  def getBoolean(boolean: Any): Boolean = boolean match {
    case boolean: Boolean => boolean
    case _ => throw TypeMismatchException(Vector())
  }
  
  def getDouble(double: Any): Double = double match {
    case double: Double => double
    case _ => throw TypeMismatchException(Vector())
  }
  
  def getString(string: Any): String = string match {
    case string: String => string
    case _ => throw TypeMismatchException(Vector())
  }
  
  def getObject(obj: Any): Map[String, Any] = obj match {
    case obj: Map[_, _] => obj collect { case (k: String, v) => k -> v }
    case _ => throw TypeMismatchException(Vector())
  }
  
  def getMutableArray(array: Any): List[Any] = array match {
    case array: ListBuffer[t] => array.to[List]
    case _ => throw TypeMismatchException(Vector())
  }

  def getMutableObject(obj: Any): Map[String, Any] = obj match {
    case obj: HashMap[_, _] => obj.asInstanceOf[HashMap[String, Any]].toMap
    case _ => throw TypeMismatchException(Vector())
  }

  def setMutableObjectValue(obj: Any, name: String, value: Any): Unit = obj match {
    case obj: HashMap[_, _] => obj.asInstanceOf[HashMap[String, Any]](name) = value
    case _ => throw TypeMismatchException(Vector())
  }
  
  def removeMutableObjectValue(obj: Any, name: String): Unit = obj match {
    case obj: HashMap[_, _] => obj.asInstanceOf[HashMap[String, Any]].remove(name)
    case _ => throw TypeMismatchException(Vector())
  }
  
  def addMutableArrayValue(array: Any, value: Any): Unit = array match {
    case array: ListBuffer[_] => array.asInstanceOf[ListBuffer[Any]] += value
    case _ => throw TypeMismatchException(Vector())
  }
  
  def setMutableArrayValue(array: Any, index: Int, value: Any): Unit = array match {
    case array: ListBuffer[_] => array.asInstanceOf[ListBuffer[Any]](index) = value
    case _ => throw TypeMismatchException(Vector())
  }
  
  
  def isBoolean(any: Any): Boolean = typeTest { case _: Boolean => () } (any)
  def isString(any: Any): Boolean = typeTest { case _: String => () } (any)
  def isNumber(any: Any): Boolean = typeTest { case _: Double => () } (any)
  def isObject(any: Any): Boolean = typeTest { case _: Map[_, _] => () } (any)
  def isArray(any: Any): Boolean = typeTest { case _: List[_] => () } (any)
  def isNull(any: Any): Boolean = any == null
  
  def isMutableArray(any: Any): Boolean = typeTest { case _: ListBuffer[_] => () } (any)
  def isMutableObject(any: Any): Boolean = typeTest { case _: HashMap[_, _] => () } (any)
  
  def toMutable(any: Any): Any = any match {
    case list: List[t] => list.map(toMutable).to[ListBuffer]
    case map: Map[k, v] => HashMap.empty ++ map.mapValues(toMutable)
    case other => other
  }
  
  def parse(s: String): Option[Any] = JSON.parseFull(s)
}

