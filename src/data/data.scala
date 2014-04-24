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

import rapture.core._

import scala.collection.mutable.{ListBuffer, HashMap}

import language.dynamics
import language.higherKinds
import language.existentials

object DataCompanion { object Empty }

trait DataCompanion[+Type <: DataType[Type, RepresentationType], -RepresentationType <: DataRepresentation] {

  def empty(implicit representation: RepresentationType) =
    construct(representation.fromObject(Map()), Vector())

  def construct(any: Any, path: Vector[Either[Int, String]])(implicit representation: RepresentationType): Type = constructRaw(Array(any), path)

  def constructRaw(any: Array[Any], path: Vector[Either[Int, String]])(implicit representation: RepresentationType): Type

  def parse[Source, R <: RepresentationType](s: Source)(implicit eh: ExceptionHandler,
      parser: Parser[Source, R]): eh.![Type, ParseException] = eh.wrap {
    construct(try parser.parse(s).get catch {
      case e: NoSuchElementException => throw new ParseException(s.toString)
    }, Vector())(parser.representation)
  }

  def apply[T: Serializer](t: T)(implicit representation: RepresentationType): Type =
    construct(implicitly[Serializer[T]].serialize(t), Vector())

  def unapply(value: Any)(implicit representation: RepresentationType): Option[Type] =
    Some(construct(value, Vector()))

  def format(value: Option[Any], ln: Int, representation: RepresentationType, pad: String,
      brk: String): String

}

trait DataType[+T <: DataType[T, RepresentationType], +RepresentationType <: DataRepresentation] extends Dynamic {
  def companion: DataCompanion[T, RepresentationType]
  protected def root: Array[Any]
  implicit def representation: RepresentationType
  def path: Vector[Either[Int, String]]
  protected def doNormalize(orEmpty: Boolean): Any
  def normalize = doNormalize(false)

  def wrap(any: Any): T

  /** Navigates the JSON using the `List[String]` parameter, and returns the element at that
    * position in the tree. */
  def normalizeOrNil: Any =
    try normalize catch { case e: Exception => representation.fromArray(List()) }

  def normalizeOrEmpty: Any =
    try normalize catch { case e: Exception => representation.fromObject(Map()) }

  def format: String = companion.format(Some(normalize), 0, representation, " ", "\n")

  def serialize: String = companion.format(Some(normalize), 0, representation, "", "")

  def apply(i: Int): T =
    companion.constructRaw(root, Left(i) +: path)

  def applyDynamic(key: String)(i: Int): T = selectDynamic(key).apply(i)

  override def equals(any: Any) = any match {
    case any: DataType[_, _] => root(0) == any.root(0)
    case _ => false
  }

  def as[S](implicit ext: Extractor[S, T], eh: ExceptionHandler): eh.![S, DataGetException]

  override def hashCode = root(0).hashCode & "json".hashCode

  /** Assumes the Json object wraps a `Map`, and extracts the element `key`. */
  def selectDynamic(key: String): T =
    companion.constructRaw(root, Right(key) +: path)

  def extract(sp: Vector[String]): DataType[T, RepresentationType] =
    if(sp.isEmpty) this else selectDynamic(sp.head).extract(sp.tail)

  override def toString = try format catch {
    case e: DataGetException => "undefined"
  }

}

trait MutableDataType[+T <: DataType[T, RepresentationType], RepresentationType <: MutableDataRepresentation]
    extends DataType[T, RepresentationType] {

  def setRoot(value: Any): Unit

  def updateParents(p: Vector[Either[Int, String]], newVal: Any): Unit = p match {
    case Vector() =>
      setRoot(newVal)
    case Left(idx) +: init =>
      val jb = companion.constructRaw(root, init)
      updateParents(init, representation.setArrayValue(jb.normalizeOrNil, idx, newVal))
    case Right(key) +: init =>
      val jb = companion.constructRaw(root, init)
      updateParents(init, representation.setObjectValue(jb.normalizeOrEmpty, key, newVal))
  }

  /** Updates the element `key` of the JSON object with the value `v` */
  def updateDynamic(key: String)(v: ForcedConversion): Unit =
    updateParents(path, representation.setObjectValue(normalizeOrEmpty, key, v.value))

  /** Updates the `i`th element of the JSON array with the value `v` */
  def update[T: Serializer](i: Int, v: T): Unit =
    updateParents(path, representation.setArrayValue(normalizeOrNil, i,
        implicitly[Serializer[T]].serialize(v)))

  /** Removes the specified key from the JSON object */
  def -=(k: String): Unit = updateParents(path, representation.removeObjectValue(doNormalize(true), k))

  /** Adds the specified value to the JSON array */
  def +=[T: Serializer](v: T): Unit = {
    val r = doNormalize(true)
    val insert = if(r == DataCompanion.Empty) representation.fromArray(Nil) else r
    updateParents(path, representation.addArrayValue(insert, implicitly[Serializer[T]].serialize(v)))
  }

}

object ForcedConversion {
  implicit def forceConversion[T: Serializer](t: T) =
    ForcedConversion(implicitly[Serializer[T]].serialize(t))
}
case class ForcedConversion(var value: Any)

