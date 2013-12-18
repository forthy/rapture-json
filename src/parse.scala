/**********************************************************************************************\
* Rapture JSON Library                                                                         *
* Version 0.9.0                                                                                *
*                                                                                              *
* The primary distribution site is                                                             *
*                                                                                              *
*   http://rapture.io/                                                                         *
*                                                                                              *
* Copyright 2010-2013 Jon Pretty, Propensive Ltd.                                              *
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

object jsonParsers {
  implicit val scalaJson = ScalaJsonParser
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

  type JsonNumber
  type JsonBoolean
  type JsonString
  type JsonObject
  type JsonArray
  type JsonNull

  def getBoolean(boolean: JsonBoolean): Boolean
  def getString(string: JsonString): String
  def getDouble(number: JsonNumber): Double
  def getInt(number: JsonNumber): Int
  def getObject(obj: JsonObject): Map[String, Any]
  def getArray(array: JsonArray): Seq[Any]
}

/** The default JSON parser implementation */
object ScalaJsonParser extends JsonParser[String] {
  
  import scala.util.parsing.json._
  
  type JsonBoolean = Boolean
  type JsonString = String
  type JsonNumber = Double
  type JsonArray = List[Any]
  type JsonObject = Map[String, Any]
  type JsonNull = Null

  def getArray(array: List[Any]) = array
  def getBoolean(boolean: Boolean) = boolean
  def getDouble(number: Double) = number
  def getInt(number: Double) = number.toInt
  def getString(string: String) = string
  def getObject(obj: Map[String, Any]) = obj
  
  def parse(s: String): Option[Any] = JSON.parseFull(s)
}

