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

import scala.util.Try

import language.dynamics
import language.higherKinds
import language.existentials

object DataCompanion { object Empty }

trait DataCompanion[+Type <: DataType[Type, DataRepresentation], -RepresentationType <: DataRepresentation] {

  def empty(implicit representation: RepresentationType) =
    constructRaw(Array(representation.fromObject(Map())), Vector())

  def constructRaw(any: Array[Any], path: Vector[Either[Int, String]])(implicit representation: RepresentationType): Type

  def parse[Source, R <: RepresentationType](s: Source)(implicit eh: ExceptionHandler,
      parser: Parser[Source, R]): eh.![Type, ParseException] = eh.wrap {
    constructRaw(try Array(parser.parse(s).get) catch {
      case e: NoSuchElementException => throw new ParseException(s.toString)
    }, Vector())(parser.representation)
  }

  def apply[T: Serializer](t: T)(implicit representation: RepresentationType): Type =
    constructRaw(Array(?[Serializer[T]].serialize(t)), Vector())

  def unapply(value: Any)(implicit representation: RepresentationType): Option[Type] =
    Some(constructRaw(Array(value), Vector()))

  def format(value: Option[Any], ln: Int, representation: RepresentationType, pad: String,
      brk: String): String

}

case class DPath(path: List[String]) extends Dynamic {
  def selectDynamic(v: String) = DPath(v :: path)
}

trait DataType[+T <: DataType[T, RepresentationType], +RepresentationType <: DataRepresentation] extends Dynamic {
  protected def root: Array[Any]
  implicit def representation: RepresentationType
  def path: Vector[Either[Int, String]]
  protected def normalize = doNormalize(false)

  protected def doNormalize(orEmpty: Boolean): Any =
    yCombinator[(Any, Vector[Either[Int, String]]), Any] { fn => _ match {
      case (j, Vector()) => j: Any
      case (j, t :+ e) =>
        fn(({
          if(e.map(x => representation.isArray(j), x => representation.isObject(j))) {
            try e.map(representation.dereferenceArray(j, _), representation.dereferenceObject(j, _)) catch {
              case TypeMismatchException(f, e, _) =>
                TypeMismatchException(f, e, path.drop(t.length))
              case e: Exception =>
                if(orEmpty) DataCompanion.Empty
                else throw MissingValueException(path.drop(t.length))
            }
          } else throw TypeMismatchException(
            if(representation.isArray(j)) DataTypes.Array else DataTypes.Object,
                e.map(l => DataTypes.Array, r => DataTypes.Object),
            path.drop(t.length)
          )
        }, t))
    } } (root(0) -> path)

  /** Assumes the Json object is wrapping a `T`, and casts (intelligently) to that type. */
  def as[S](implicit ext: Extractor[S, T], eh: ExceptionHandler): eh.![S, DataGetException] =
    eh wrap {
      try ext.construct(wrap(normalize)) catch {
        case TypeMismatchException(f, e, _) => throw TypeMismatchException(f, e, path)
        case e: MissingValueException => throw e
      }
    }

  def wrap(any: Any, path: Vector[Either[Int, String]] = Vector()): T

  def format: String

  def serialize: String

  def apply(i: Int): T = wrap(root(0), Left(i) +: path)

  def applyDynamic(key: String)(i: Int): T = selectDynamic(key).apply(i)

  override def equals(any: Any) = any match {
    case any: DataType[_, _] => root(0) == any.root(0)
    case _ => false
  }

  override def hashCode = root(0).hashCode & "json".hashCode

  /** Assumes the Json object wraps a `Map`, and extracts the element `key`. */
  def selectDynamic(key: String): T = wrap(root(0), Right(key) +: path)

  def extract(sp: Vector[String]): DataType[T, RepresentationType] =
    if(sp.isEmpty) this else selectDynamic(sp.head).extract(sp.tail)

  override def toString = try format catch {
    case e: DataGetException => "undefined"
  }

  def ++[S <: DataType[S, Rep] forSome { type Rep }](b: S): T = {
    def merge(a: Any, b: Any): Any = {
      if(representation.isObject(b)) {
        if(representation.isObject(a)) {
          representation.fromObject(representation.getKeys(b).foldLeft(representation.getObject(a)) { case (as, k) =>
            as + (k -> {
              if(as contains k) merge(as(k), representation.dereferenceObject(b, k))
              else representation.dereferenceObject(b, k)
            })
          })
        } else b
      } else if(representation.isArray(b)) {
        if(representation.isArray(a)) representation.fromArray(representation.getArray(a) ++ representation.getArray(b))
        else b
      } else b
    }
    wrap(merge(normalize, b.root(0)), Vector())
  }

  def +(pv: (DPath => DPath, ForcedConversion)) = {
    def add(path: List[String], v: Any): Any = path match {
      case Nil => v
      case next :: list => representation.fromObject(Map(next -> add(list, v)))
    }
    this ++ wrap(add(pv._1(DPath(Nil)).path.reverse, pv._2.value), Vector())
  }
}

trait MutableDataType[+T <: DataType[T, RepresentationType], RepresentationType <: MutableDataRepresentation]
    extends DataType[T, RepresentationType] {

  def setRoot(value: Any): Unit

  def updateParents(p: Vector[Either[Int, String]], newVal: Any): Unit = p match {
    case Vector() =>
      setRoot(newVal)
    case Left(idx) +: init =>
      val jb = wrap(root(0), init)
      updateParents(init, representation.setArrayValue(Try(jb.normalize).getOrElse(Nil), idx, newVal))
    case Right(key) +: init =>
      val jb = wrap(root(0), init)
      updateParents(init, representation.setObjectValue(Try(jb.normalize).getOrElse(Map()), key, newVal))
  }

  /** Updates the element `key` of the JSON object with the value `v` */
  def updateDynamic(key: String)(v: ForcedConversion): Unit =
    updateParents(path, representation.setObjectValue(Try(normalize).getOrElse(Map()), key, v.value))

  /** Updates the `i`th element of the JSON array with the value `v` */
  def update[T: Serializer](i: Int, v: T): Unit =
    updateParents(path, representation.setArrayValue(Try(normalize).getOrElse(Nil), i,
        ?[Serializer[T]].serialize(v)))

  /** Removes the specified key from the JSON object */
  def -=(k: String): Unit = updateParents(path, representation.removeObjectValue(doNormalize(true), k))

  /** Adds the specified value to the JSON array */
  def +=[T: Serializer](v: T): Unit = {
    val r = doNormalize(true)
    val insert = if(r == DataCompanion.Empty) representation.fromArray(Nil) else r
    updateParents(path, representation.addArrayValue(insert, ?[Serializer[T]].serialize(v)))
  }

}

object ForcedConversion {
  implicit def forceConversion[T: Serializer](t: T) =
    ForcedConversion(?[Serializer[T]].serialize(t))
}
case class ForcedConversion(var value: Any)

