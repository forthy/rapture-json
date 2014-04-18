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

import scala.reflect.macros._
import scala.annotation._

import language.experimental.macros
import language.higherKinds

object Macros {
 
  def extractorMacro[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[Extractor[T]] = {
    import c.universe._

    require(weakTypeOf[T].typeSymbol.asClass.isCaseClass)

    val extractor = typeOf[Extractor[_]].typeSymbol.asType.toTypeConstructor

    // FIXME: This will perform badly for large objects, as the map extraction is applied to
    // all elements once for every element
    val params = weakTypeOf[T].decls collect {
      case m: MethodSymbol if m.isCaseAccessor => m.asMethod
    } map { p =>

      val paramValue = c.Expr[Any](Apply(
        Select(
          Ident(TermName("json")),
          TermName("$accessInnerJsonMap")
        ),
        List(Literal(Constant(p.name.toString)))
      ))

      val parserValue = c.Expr[JsonParser[_]](Select(
        Ident(TermName("json")),
        TermName("parser")
      ))

      val newArray = reify(new Json(Array(paramValue.splice))(parserValue.splice))

      Apply(
        Select(
          c.Expr[Extractor[_]](
            c.inferImplicitValue(appliedType(extractor, List(p.returnType)), false, false)
          ).tree,
          TermName("construct")
        ),
        List(newArray.tree)
      )
    }


    val construction = c.Expr(
      Apply(
        Select(
          New(
            TypeTree(weakTypeOf[T])
          ),
          termNames.CONSTRUCTOR
        ),
        params.to[List]
      )
    )

    reify(new Extractor[T] { def construct(json: Json): T = construction.splice })
  }

  def jsonizerMacro[T: c.WeakTypeTag](c: blackbox.Context)(parser: c.Expr[JsonParser[_]]): c.Expr[Jsonizer[T]] = {
    import c.universe._

    val tpe = weakTypeOf[T].typeSymbol.asClass
    val jsonizer = typeOf[Jsonizer[_]].typeSymbol.asType.toTypeConstructor

    val construction = if(tpe.isCaseClass) {

      val params = weakTypeOf[T].decls collect {
        case m: MethodSymbol if m.isCaseAccessor => m.asMethod
      } map { p =>
        Apply(
          Select(
            Apply(
              Select(
                Ident(definitions.PredefModule),
                TermName("any2ArrowAssoc")
              ),
              List(
                Literal(Constant(p.name.toString))
              )
            ),
            TermName("$minus$greater")
          ),
          List(
            Apply(
              Select(
                c.inferImplicitValue(appliedType(jsonizer, List(p.returnType)), false, false),
                TermName("jsonize")
              ),
              List(
                Select(
                  Ident(TermName("t")),
                  p.name
                )
              )
            )
          )
        )
      }

      c.Expr[Map[String, Any]](
        Apply(
          Select(
            Select(
              Ident(definitions.PredefModule),
              TermName("Map")
            ),
            TermName("apply")
          ),
          params.to[List]
        )
      )
    } else if(tpe.isSealed) {
      c.Expr[Map[String, Any]](
        Match(
          Ident(TermName("t")),
          tpe.knownDirectSubclasses.to[List] map { sc =>
            CaseDef(
              Bind(
                TermName("v"),
                Typed(
                  Ident(termNames.WILDCARD),
                  Ident(sc.asClass)
                )
              ),
              EmptyTree,
              Apply(
                Select(
                  c.inferImplicitValue(appliedType(jsonizer, List(sc.asType.toType)), false, false),
                  TermName("jsonize")
                ),
                List(Ident(TermName("v")))
              )
            )
          }
        )
      )
    } else throw new Exception()

    reify(new Jsonizer[T] { def jsonize(t: T): Any = parser.splice.fromObject(construction.splice) })
  }
}

object Extractor {

  implicit val jsonExtractor: Extractor[Json] = BasicExtractor[Json](identity)
  
  implicit val stringExtractor: Extractor[String] = BasicExtractor[String](x =>
      x.parser.getString(x.root(0)))
  
  implicit val doubleExtractor: Extractor[Double] = BasicExtractor[Double](x =>
      x.parser.getDouble(x.root(0)))
  
  implicit val floatExtractor: Extractor[Float] = BasicExtractor[Float](x =>
      x.parser.getDouble(x.root(0)).toFloat)

  implicit val shortExtractor: Extractor[Short] = BasicExtractor[Short](x =>
      x.parser.getDouble(x.root(0)).toShort)

  implicit val intExtractor: Extractor[Int] = BasicExtractor[Int](x =>
      x.parser.getDouble(x.root(0)).toInt)

  implicit val longExtractor: Extractor[Long] = BasicExtractor[Long](x =>
      x.parser.getDouble(x.root(0)).toLong)

  implicit val byteExtractor: Extractor[Byte] = BasicExtractor[Byte](x =>
      x.parser.getDouble(x.root(0)).toInt.toByte)

  implicit val booleanExtractor: Extractor[Boolean] = BasicExtractor[Boolean](x =>
      x.parser.getBoolean(x.root(0)))

  implicit val anyExtractor: Extractor[Any] = BasicExtractor[Any](_.root(0))
  
  implicit def genSeqExtractor[T, Coll[_]](implicit cbf:
      scala.collection.generic.CanBuildFrom[Nothing, T, Coll[T]], ext: Extractor[T]):
      Extractor[Coll[T]] =
    BasicExtractor[Coll[T]]({ x =>
      x.parser.getArray(x.root(0)).to[List].map(v => ext.construct(new Json(Array(v))(x.parser))).to[Coll]
    })

  implicit def optionExtractor[T](implicit ext: Extractor[T]): Extractor[Option[T]] =
    new BasicExtractor[Option[T]](x =>
      if(x.root(0) == null) None else Some(x.root(0): Any) map (v => ext.construct(new Json(Array(v))(x.parser)))
    ) { override def errorToNull = true }
  
  implicit def mapExtractor[T](implicit ext: Extractor[T]): Extractor[Map[String, T]] =
    BasicExtractor[Map[String, T]](x =>
      x.parser.getObject(x.root(0)) mapValues (v => ext.construct(new Json(Array(v))(x.parser)))
    )
}

@implicitNotFound("cannot extract type ${T} from JSON.")
trait Extractor[T] {
  def construct(any: Json): T
  //def rawConstruct(any: Any, parser: JsonParser[_]): T = construct(new Json(Array(any))(parser))
  def errorToNull = false
}


object Jsonizer {

  implicit def identityJsonizer(implicit parser: JsonParser[_]): Jsonizer[Json] =
    new Jsonizer[Json] { def jsonize(j: Json) = j.root(0) }
  
  implicit def intJsonizer(implicit parser: JsonParser[_]): Jsonizer[Int] =
    new Jsonizer[Int] { def jsonize(i: Int) = parser.fromDouble(i.toDouble) }
  
  implicit def booleanJsonizer(implicit parser: JsonParser[_]): Jsonizer[Boolean] =
    new Jsonizer[Boolean] { def jsonize(b: Boolean) = parser.fromBoolean(b) }
  
  implicit def stringJsonizer(implicit parser: JsonParser[_]): Jsonizer[String] =
    new Jsonizer[String] { def jsonize(s: String) = parser.fromString(s) }
  
  implicit def floatJsonizer(implicit parser: JsonParser[_]): Jsonizer[Float] =
    new Jsonizer[Float] { def jsonize(f: Float) = parser.fromDouble(f.toDouble) }
  
  implicit def doubleJsonizer(implicit parser: JsonParser[_]): Jsonizer[Double] =
    new Jsonizer[Double] { def jsonize(d: Double) = parser.fromDouble(d) }
  
  implicit def longJsonizer(implicit parser: JsonParser[_]): Jsonizer[Long] =
    new Jsonizer[Long] { def jsonize(l: Long) = parser.fromDouble(l.toDouble) }
  
  implicit def shortJsonizer(implicit parser: JsonParser[_]): Jsonizer[Short] =
    new Jsonizer[Short] { def jsonize(s: Short) = parser.fromDouble(s.toDouble) }
  
  implicit def byteJsonizer(implicit parser: JsonParser[_]): Jsonizer[Byte] =
    new Jsonizer[Byte] { def jsonize(b: Byte) = parser.fromDouble(b.toDouble) }
  
  implicit def listJsonizer[T: Jsonizer](implicit parser: JsonParser[_]): Jsonizer[List[T]] =
    new Jsonizer[List[T]] { def jsonize(xs: List[T]) = parser.fromArray(xs.map(implicitly[Jsonizer[T]].jsonize)) }
  
  implicit def genSeqJsonizer[T: Jsonizer](implicit parser: JsonParser[_]): Jsonizer[Traversable[T]] =
    new Jsonizer[Traversable[T]] {
      def jsonize(xs: Traversable[T]) =
        parser.fromArray(xs.map(implicitly[Jsonizer[T]].jsonize).to[List])
    }
  
  implicit def mapJsonizer[T: Jsonizer](implicit parser: JsonParser[_]): Jsonizer[Map[String, T]] =
    new Jsonizer[Map[String, T]] {
      def jsonize(m: Map[String, T]) = parser.fromObject(m.mapValues(implicitly[Jsonizer[T]].jsonize))
    }
  
}

@implicitNotFound("cannot serialize type ${T} to JSON.")
trait Jsonizer[T] {
  def jsonize(t: T): Any
}
