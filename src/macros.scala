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

import scala.reflect.macros._
import scala.annotation._

import language.experimental.macros
import language.higherKinds

object Macros {

  def extractorMacro[T: c.WeakTypeTag](c: BlackboxContext): c.Expr[Unwrapper[T]] = {
    import c.universe._

    require(weakTypeOf[T].typeSymbol.asClass.isCaseClass)

    val extractor = typeOf[Unwrapper[_]].typeSymbol.asType.toTypeConstructor

    // FIXME: This will perform badly for large objects, as the map extraction is applied to
    // all elements once for every element
    val params = weakTypeOf[T].declarations collect {
      case m: MethodSymbol if m.isCaseAccessor => m.asMethod
    } map { p =>
      val c1 = Apply(
        Select(
          c.Expr[Unwrapper[_]](
            c.inferImplicitValue(appliedType(extractor, List(p.returnType)), false, false)
          ).tree,
          TermName("unwrap")
        ),
        List(
          Apply(
            Select(
              Ident(TermName("json")),
              TermName("$accessInnerJsonMap")
            ),
            List(Literal(Constant(p.name.toString)))
          ),
          Select(
            Ident(TermName("json")),
            TermName("parser")
          )
        )
      )

      Apply(
        Select(
          Select(
            Ident(TermName("json")),
            TermName("companion")
          ),
          TermName("construct")
        ),
        List(c1)
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

    reify(new Unwrapper[T] { def unwrap(json: Json): T = construction.splice })
  }

  def jsonizerMacro[T: c.WeakTypeTag](c: BlackboxContext)(parser: c.Expr[JsonParser[_]]): c.Expr[Wrapper[T]] = {
    import c.universe._

    val tpe = weakTypeOf[T].typeSymbol.asClass
    val wrapper = typeOf[Wrapper[_]].typeSymbol.asType.toTypeConstructor

    val construction = if(tpe.isCaseClass) {

      val params = weakTypeOf[T].declarations collect {
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
                c.inferImplicitValue(appliedType(wrapper, List(p.returnType)), false, false),
                TermName("wrap")
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
                  Ident(nme.WILDCARD),
                  Ident(sc.asClass)
                )
              ),
              EmptyTree,
              Apply(
                Select(
                  c.inferImplicitValue(appliedType(wrapper, List(sc.asType.toType)), false, false),
                  TermName("wrap")
                ),
                List(Ident(TermName("v")))
              )
            )
          }
        )
      )
    } else throw new Exception()

    reify(new Wrapper[T] { def wrap(t: T): Any = parser.splice.fromObject(construction.splice) })
  }
}
