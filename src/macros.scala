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

import scala.reflect.macros._
import scala.annotation._

import language.experimental.macros

object Macros {
 
  def extractorMacro[T: c.WeakTypeTag](c: Context): c.Expr[Extractor[T]] = {
    import c.universe._

    require(weakTypeOf[T].typeSymbol.asClass.isCaseClass)

    val extractor = typeOf[Extractor[_]].typeSymbol.asType.toTypeConstructor

    // FIXME: This will perform badly for large objects, as the map extraction is applied to
    // all elements once for every element
    val params = weakTypeOf[T].declarations collect {
      case m: MethodSymbol if m.isCaseAccessor => m.asMethod
    } map { p =>
      Apply(
        Select(
          c.Expr[Extractor[_]](
            c.inferImplicitValue(appliedType(extractor, List(p.returnType)), false, false)
          ).tree,
          newTermName("rawConstruct")
        ),
        List(
          Apply(
            Select(
              Ident(newTermName("json")),
              newTermName("$accessInnerJsonMap")
            ),
            List(Literal(Constant(p.name.toString)))
          ),
          Select(
            Ident(newTermName("json")),
            newTermName("parser")
          )
        )
      )
    }

    val construction = c.Expr(
      Apply(
        Select(
          New(
            TypeTree(weakTypeOf[T])
          ),
          nme.CONSTRUCTOR
        ),
        params.to[List]
      )
      /*New(
        weakTypeOf[T].typeSymbol,
        params.to[List]: _*
      )*/
    )

    reify(new Extractor[T] { def construct(json: Json): T = construction.splice })
  }

  def jsonizerMacro[T: c.WeakTypeTag](c: Context): c.Expr[Jsonizer[T]] = {
    import c.universe._

    val tpe = weakTypeOf[T].typeSymbol.asClass
    val jsonizer = typeOf[Jsonizer[_]].typeSymbol.asType.toTypeConstructor

    val construction = if(tpe.isCaseClass) {

      val params = weakTypeOf[T].declarations collect {
        case m: MethodSymbol if m.isCaseAccessor => m.asMethod
      } map { p =>
        Apply(
          Select(
            Apply(
              Select(
                Ident(definitions.PredefModule),
                newTermName("any2ArrowAssoc")
              ),
              List(
                Literal(Constant(p.name.toString))
              )
            ),
            newTermName("$minus$greater")
          ),
          List(
            Apply(
              Select(
                c.inferImplicitValue(appliedType(jsonizer, List(p.returnType)), false, false),
                newTermName("jsonize")
              ),
              List(
                Select(
                  Ident(newTermName("t")),
                  p.name
                )
              )
            )
          )
        )
      }

      c.Expr(
        Apply(
          Select(
            Select(
              Ident(definitions.PredefModule),
              newTermName("Map")
            ),
            newTermName("apply")
          ),
          params.to[List]
        )
      )
    } else if(tpe.isSealed) {
      c.Expr(
        Match(
          Ident(newTermName("t")),
          tpe.knownDirectSubclasses.to[List] map { sc =>
            CaseDef(
              Bind(
                newTermName("v"),
                Typed(
                  Ident(nme.WILDCARD),
                  Ident(sc.asClass)
                )
              ),
              EmptyTree,
              Apply(
                Select(
                  c.inferImplicitValue(appliedType(jsonizer, List(sc.asType.toType)), false, false),
                  newTermName("jsonize")
                ),
                List(Ident(newTermName("v")))
              )
            )
          }
        )
      )
    } else throw new Exception()

    reify(new Jsonizer[T] { def jsonize(t: T): Any = construction.splice })
  }
}

@implicitNotFound("cannot extract type ${T} from JSON.")
trait Extractor[T] {
  def construct(any: Json): T
  def rawConstruct(any: Any, parser: JsonParser[_]): T = construct(new Json(any)(parser))
  def errorToNull = false
}

@implicitNotFound("cannot serialize type ${T} to JSON.")
trait Jsonizer[T] {
  def jsonize(t: T): Any
}
