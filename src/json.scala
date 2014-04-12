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
import rapture.data._

import language.{dynamics, higherKinds}

trait JsonDataType[+T <: JsonDataType[T]]
    extends DataType[T] {

  type Parser[-S] <: companion.Parser[S]
  type Companion <: JsonDataCompanion[T]

  protected def doNormalize(orEmpty: Boolean): Any =
    yCombinator[(Any, Vector[Either[Int, String]]), Any] { fn => _ match {
      case (j, Vector()) => j: Any
      case (j, t :+ Right(k)) =>
        fn(({
          if(parser.isObject(j)) {
            try parser.dereferenceObject(j, k) catch {
              case TypeMismatchException(f, e, _) =>
                TypeMismatchException(f, e, path.drop(t.length))
              case e: Exception =>
                if(orEmpty) DataCompanion.Empty
                else throw MissingValueException(path.drop(t.length))
            }
          } else {
            throw TypeMismatchException(parser.getType(j), DataTypes.Object,
              path.drop(t.length))
          }
        }, t))
      case (j, t :+ Left(i)) =>
        fn((
          if(parser.isArray(j)) {
            try parser.dereferenceArray(j, i) catch {
              case TypeMismatchException(f, e, _) =>
                TypeMismatchException(f, e, path.drop(t.length))
              case e: Exception =>
                if(orEmpty) DataCompanion.Empty
                else throw MissingValueException(path.drop(t.length))
            }
          } else {
            throw TypeMismatchException(parser.getType(j), DataTypes.Array, path.drop(t.length))
          }
        , t))
    } } (root(0) -> path)
  
}

trait JsonDataCompanion[T] extends DataCompanion[JsonDataType[T]] {
  override type Parser[-S] = JsonParser[S]
}

trait JsonUnwrappers[+T <: JsonDataType[T]] { this: JsonDataCompanion[T] =>

  case class BasicUnwrapper[V](val cast: T => V) extends Unwrapper[V] {
    def unwrap(js: T) = cast(js)
  }

  implicit val jsonUnwrapper: Unwrapper[T] = BasicUnwrapper[T](identity)

  implicit val stringUnwrapper: Unwrapper[String] = BasicUnwrapper[String](x =>
      x.parser.getString(x.root(0)))

  implicit val doubleUnwrapper: Unwrapper[Double] = BasicUnwrapper[Double](x =>
      x.parser.getDouble(x.root(0)))

  implicit val floatUnwrapper: Unwrapper[Float] = BasicUnwrapper[Float](x =>
      x.parser.getDouble(x.root(0)).toFloat)

  implicit val shortUnwrapper: Unwrapper[Short] = BasicUnwrapper[Short](x =>
      x.parser.getDouble(x.root(0)).toShort)

  implicit val intUnwrapper: Unwrapper[Int] = BasicUnwrapper[Int](x =>
      x.parser.getDouble(x.root(0)).toInt)

  implicit val longUnwrapper: Unwrapper[Long] = BasicUnwrapper[Long](x =>
      x.parser.getDouble(x.root(0)).toLong)

  implicit val byteUnwrapper: Unwrapper[Byte] = BasicUnwrapper[Byte](x =>
      x.parser.getDouble(x.root(0)).toInt.toByte)

  implicit val booleanUnwrapper: Unwrapper[Boolean] = BasicUnwrapper[Boolean](x =>
      x.parser.getBoolean(x.root(0)))

  implicit val anyUnwrapper: Unwrapper[Any] = BasicUnwrapper[Any](_.root(0))

  implicit def genSeqUnwrapper[V, Coll[_]](implicit cbf:
      scala.collection.generic.CanBuildFrom[Nothing, V, Coll[V]], unwrapper: Unwrapper[V]):
      Unwrapper[Coll[V]] =
    BasicUnwrapper[Coll[V]]({ x =>
      x.parser.getArray(x.root(0)).to[List].map(j => unwrapper.unwrap(construct(j, Vector())(x.parser.asInstanceOf[Parser[_]]))).to[Coll]
    })

  implicit def optionUnwrapper[V](implicit unwrapper: Unwrapper[V]): Unwrapper[Option[V] ] =
    new BasicUnwrapper[Option[V]](x =>
      if(x.root(0) == null) None else Some(x.root(0): Any) map (j => unwrapper.unwrap(construct(j, Vector())(x.parser.asInstanceOf[Parser[_]])))
    ) { override def errorToNull = true }

  implicit def mapUnwrapper[V](implicit unwrapper: Unwrapper[V]): Unwrapper[Map[String, V]] =
    BasicUnwrapper[Map[String, V]](x =>
      x.parser.getObject(x.root(0)) mapValues (j => unwrapper.unwrap(construct(j, Vector())(x.parser.asInstanceOf[Parser[_]])))
    )
}

/** Companion object to the `Json` type, providing factory and unwrapper methods, and a JSON
  * pretty printer. */
object Json extends JsonDataCompanion[Json] with JsonUnwrappers[Json] {
  type Parser[-S] = JsonParser[S]
  def constructRaw(any: Array[Any], path: Vector[Either[Int, String]])(implicit parser: JsonParser[_]): Json =
    new Json(any, path)

}

/** Represents some parsed JSON. */
class Json(val root: Array[Any], val path: Vector[Either[Int, String]] = Vector())(implicit
    parserInit: JsonParser[_]) extends JsonDataType[Json] {
  override type Parser[-Src] = companion.Parser[Src]
  type Companion = JsonDataCompanion[Json]
  def parser: Parser[_] = parserInit
  override val companion: Companion = Json
  def $accessInnerJsonMap(k: String): Any = parser.dereferenceObject(root(0), k)

  /** Formats the JSON object for multi-line readability. */
  def format(json: Option[Any], ln: Int, pad: String = " ", brk: String = "\n"): String = {
    val indent = " "*ln
    json match {
      case None => "undefined"
      case Some(j) =>
        if(parser.isString(j)) {
          "\""+parser.getString(j).replaceAll("\\\\", "\\\\\\\\").replaceAll("\r",
              "\\\\r").replaceAll("\n", "\\\\n").replaceAll("\"", "\\\\\"")+"\""
        } else if(parser.isBoolean(j)) {
          if(parser.getBoolean(j)) "true" else "false"
        } else if(parser.isNumber(j)) {
          val n = parser.getDouble(j)
          if(n == n.toInt) n.toInt.toString else n.toString
        } else if(parser.isArray(j)) {
          List("[", parser.getArray(j) map { v =>
            s"${indent}${pad}${format(Some(v), ln + 1)}"
          } mkString s",${brk}", s"${indent}]") mkString brk
        } else if(parser.isObject(j)) {
          List("{", parser.getKeys(j) map { k =>
            val inner = try Some(parser.dereferenceObject(j, k)) catch {
              case e: Exception => None
            }
            s"""${indent}${pad}"${k}":${pad}${format(inner, ln + 1)}"""
          } mkString s",${brk}", s"${indent}}") mkString brk
        } else if(parser.isNull(j)) "null"
        else if(j == DataCompanion.Empty) "empty"
        else "undefined"
    }
  }
}

object JsonBuffer extends JsonDataCompanion[JsonBuffer] with MutableDataCompanion[JsonBuffer] with JsonUnwrappers[JsonBuffer] {
  type Parser[-S] = JsonBufferParser[S]
  def constructRaw(any: Array[Any], path: Vector[Either[Int, String]])(implicit parser: JsonBufferParser[_]): JsonBuffer = new JsonBuffer(any, path)
}

class JsonBuffer(protected val root: Array[Any], val path: Vector[Either[Int, String]] =
    Vector())(implicit parserInit: JsonBufferParser[_]) extends JsonDataType[JsonBuffer] with
    MutableDataType[JsonBuffer] {
  override type Parser[-S] = companion.Parser[S]
  val companion = JsonBuffer
  def setRoot(value: Any) = root(0) = value
  implicit override val parser: Parser[_] = parserInit
  
  /** Formats the JSON object for multi-line readability. */
  def format(json: Option[Any], ln: Int, pad: String = " ", brk: String = "\n"): String = {
    val indent = " "*ln
    json match {
      case None => "undefined"
      case Some(j) =>
        if(parser.isString(j)) {
          "\""+parser.getString(j).replaceAll("\\\\", "\\\\\\\\").replaceAll("\r",
              "\\\\r").replaceAll("\n", "\\\\n").replaceAll("\"", "\\\\\"")+"\""
        } else if(parser.isBoolean(j)) {
          if(parser.getBoolean(j)) "true" else "false"
        } else if(parser.isNumber(j)) {
          val n = parser.getDouble(j)
          if(n == n.toInt) n.toInt.toString else n.toString
        } else if(parser.isArray(j)) {
          List("[", parser.getArray(j) map { v =>
            s"${indent}${pad}${format(Some(v), ln + 1)}"
          } mkString s",${brk}", s"${indent}]") mkString brk
        } else if(parser.isObject(j)) {
          List("{", parser.getKeys(j) map { k =>
            val inner = try Some(parser.dereferenceObject(j, k)) catch {
              case e: Exception => None
            }
            s"""${indent}${pad}"${k}":${pad}${format(inner, ln + 1)}"""
          } mkString s",${brk}", s"${indent}}") mkString brk
        } else if(parser.isNull(j)) "null"
        else if(j == DataCompanion.Empty) "empty"
        else "undefined"
    }
  }
}

case class CascadeUnwrapper[T](casts: (Json => T)*) extends Unwrapper[T] {
  def unwrap(js: Json) = {
    (casts.foldLeft(None: Option[T]) { case (v, next) =>
      v orElse { try Some(next(js)) catch { case e: Exception => None } }
    }).get
  }
}

