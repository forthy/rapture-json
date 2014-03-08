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

object jsonParsers {
  implicit val scalaJson = ScalaJsonParser
  implicit val jackson = JacksonParser
  implicit val jawn = JawnParser
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

  def getBoolean(boolean: Any): Boolean
  def getString(string: Any): String
  def getDouble(number: Any): Double
  def getObject(obj: Any): Map[String, Any]
  def getArray(array: Any): Seq[Any]

  def dereferenceObject(obj: Any, element: String): Any =
    getObject(obj)(element)
  
  def getKeys(obj: Any): Iterator[String] =
    getObject(obj).keys.iterator
  
  def dereferenceArray(array: Any, element: Int): Any =
    getArray(array)(element)

  def isBoolean(any: Any): Boolean
  def isString(any: Any): Boolean
  def isNumber(any: Any): Boolean
  def isObject(any: Any): Boolean
  def isArray(any: Any): Boolean
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

/** A type class for Jackson parsing */
object JacksonParser extends JsonParser[String] {
  
  import org.codehaus.jackson
  import jackson._
  import scala.collection.JavaConversions._

  private val mapper = new map.ObjectMapper()
  
  def getArray(array: Any): List[Any] = array match {
    case list: JsonNode if list.isArray => list.getElements.to[List]
    case _ => throw TypeMismatchException(Vector())
  }

  def getBoolean(boolean: Any): Boolean = boolean match {
    case boolean: JsonNode if boolean.isBoolean => boolean.asBoolean
    case _ => throw TypeMismatchException(Vector())
  }
  
  def getDouble(any: Any): Double = any match {
    case number: JsonNode if number.isNumber => number.asDouble
    case _ => throw TypeMismatchException(Vector())
  }
  
  def getString(string: Any): String = string match {
    case string: JsonNode if string.isTextual => string.asText
    case _ => throw TypeMismatchException(Vector())
  }
  
  def getObject(obj: Any): Map[String, Any] = obj match {
    case obj: JsonNode if obj.isObject =>
      (obj.getFieldNames map { case k => k -> obj.get(k) }).toMap
    case _ => throw TypeMismatchException(Vector())
  }
  
  override def dereferenceObject(obj: Any, element: String): Any = obj match {
    case obj: JsonNode if obj.isObject => obj.get(element)
    case _ => throw TypeMismatchException(Vector())
  }

  override def getKeys(obj: Any): Iterator[String] = obj match {
    case obj: JsonNode if obj.isObject => obj.getFieldNames.to[Iterator]
    case _ => throw TypeMismatchException(Vector())
  }

  override def dereferenceArray(array: Any, element: Int): Any = array match {
    case array: JsonNode if array.isArray=> array.get(element)
    case _ => throw TypeMismatchException(Vector())
  }

  def getMutableArray(array: JsonNode): JsonNode = array match {
    case array: JsonNode => array
    case _ => throw TypeMismatchException(Vector())
  }

  def getMutableObject(obj: JsonNode): Map[String, Any] = obj match {
    case obj: JsonNode if obj.isObject =>
      (obj.getFieldNames map { case k => k -> obj.get(k) }).toMap
    case _ => throw TypeMismatchException(Vector())
  }

  def setMutableObjectValue(obj: JsonNode, name: String, value: JsonNode): Unit = ???
  
  def removeMutableObjectValue(obj: JsonNode, name: String): Unit = ???
  
  def addMutableArrayValue(array: JsonNode, value: JsonNode): Unit = ???
  
  def setMutableArrayValue(array: JsonNode, index: Int, value: JsonNode): Unit = ???

  def isBoolean(any: Any): Boolean = any match {
    case x: JsonNode if x.isBoolean => true
    case _ => false
  }
  
  def isString(any: Any): Boolean = any match {
    case x: JsonNode if x.isTextual => true
    case _ => false
  }

  def isNumber(any: Any): Boolean = any match {
    case x: JsonNode if x.isNumber => true
    case _ => false
  }
  
  def isObject(any: Any): Boolean = any match {
    case x: JsonNode if x.isObject => true
    case _ => false
  }
  
  def isArray(any: Any): Boolean = any match {
    case x: JsonNode if x.isArray => true
    case _ => false
  }
  
  def isNull(any: Any): Boolean = any match {
    case x: JsonNode if x.isNull => true
    case _ => false
  }
  
  def isMutableArray(any: JsonNode): Boolean = 
    typeTest { case n: JsonNode if n.isArray => () } (any)
  
  def isMutableObject(any: JsonNode): Boolean =
    typeTest { case n: JsonNode if n.isObject => () } (any)
  
  def toMutable(any: JsonNode): JsonNode = any
  
  def parse(s: String): Option[Any] = Some(mapper.readTree(s))
}

object JawnParser extends JsonBufferParser[String] {
 
  import jawn._

  override def dereferenceObject(obj: Any, element: String): Any =
    obj match {
      case JObject(obj) => obj(element)
    }
  
  override def getKeys(obj: Any): Iterator[String] =
    obj match {
      case JObject(obj) => obj.keys.iterator
    }
  
  override def dereferenceArray(array: Any, element: Int): Any =
    array match {
      case JArray(arr) => arr(element)
    }

  def getArray(array: Any): List[Any] = array match {
    case JArray(xs)=> xs.toList
    case _ => throw TypeMismatchException(Vector())
  }

  def getBoolean(boolean: Any): Boolean = boolean match {
    case boolean: Boolean => boolean
    case JTrue => true
    case JFalse => false
    case _ => throw TypeMismatchException(Vector())
  }
  
  def getDouble(double: Any): Double = double match {
    case DoubleNum(d) => d
    case LongNum(v) => v.toDouble
    case DeferNum(v) => v.toDouble
    case _ => throw TypeMismatchException(Vector())
  }
  
  def getString(string: Any): String = string match {
    case JString(s) => s
    case _ => throw TypeMismatchException(Vector())
  }
  
  def getObject(obj: Any): Map[String, Any] = obj match {
    case JObject(o) => o.toMap
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
  
  def isArray(array: Any): Boolean = array match {
    case JArray(xs)=> true
    case _ => false
  }

  def isBoolean(boolean: Any): Boolean = boolean match {
    case JTrue | JFalse => true
    case _ => false
  }
  
  def isNumber(num: Any): Boolean = num match {
    case DoubleNum(_) | LongNum(_) | DeferNum(_) => true
    case _ => false
  }
  
  def isString(string: Any): Boolean = string match {
    case JString(_) => true
    case _ => false
  }
  
  def isObject(obj: Any): Boolean = obj match {
    case JObject(_) => true
    case _ => false
  }
  
  def isNull(obj: Any): Boolean = obj match {
    case JNull => true
    case _ => false
  }
  
  
  def isMutableArray(any: Any): Boolean = typeTest { case _: ListBuffer[_] => () } (any)
  def isMutableObject(any: Any): Boolean = typeTest { case _: HashMap[_, _] => () } (any)
  
  def toMutable(any: Any): Any = any match {
    case list: List[t] => list.map(toMutable).to[ListBuffer]
    case map: Map[k, v] => HashMap.empty ++ map.mapValues(toMutable)
    case other => other
  }
  
  def parse(s: String): Option[Any] = jawn.JParser.parseFromString(s).right.toOption
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

