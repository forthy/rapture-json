/**********************************************************************************************\
* Rapture I/O Library                                                                          *
* Version 0.8.2                                                                                *
*                                                                                              *
* The primary distribution site is                                                             *
*                                                                                              *
*   http://rapture.io/                                                                         *
*                                                                                              *
* Copyright 2010-2013 Propensive Ltd.                                                          *
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
package rapture

import scala.reflect._
import scala.reflect.api._
import scala.reflect.runtime._
import scala.reflect.macros._
import scala.annotation._

object CaseClassExtraction {
  
  import language.experimental.macros
  
  def materialize[T: c.WeakTypeTag](c: Context): c.Expr[Extractor[T]] = {
    import c.universe._

    val extractor = typeOf[Extractor[_]].typeSymbol.asType.toTypeConstructor

    val params = weakTypeOf[T].declarations collect {
      case m: MethodSymbol if m.isCaseAccessor => m.asMethod
    } map { p =>
      Apply(
        Select(
          c.Expr[Extractor[_]](
            c.inferImplicitValue(appliedType(extractor, List(p.returnType)), false, false)
          ).tree,
          newTermName("construct")
        ),
        List(Apply(
          Select(
            TypeApply(
              Select(Ident(newTermName("json")), newTermName("asInstanceOf")),
              List(AppliedTypeTree(
                Select(Ident(definitions.PredefModule), newTypeName("Map")),
                List(
                  Select(
                    Ident(definitions.PredefModule),
                    newTypeName("String")
                  ),
                  Ident(definitions.AnyClass)
                ))
              )
            ),
            newTermName("apply")
          ),
          List(Literal(Constant(p.name.toString)))
        ))
      )
    }

    val construction = c.Expr(
      New(
        weakTypeOf[T].typeSymbol.asType,
        params.to[List]: _*
      )
    )

    reify(new Extractor[T] { def construct(json: Any): T = construction.splice })
  }
}


@implicitNotFound("Cannot extract type ${T} from JSON.")
trait Extractor[T] {
  def construct(any: Any): T
  def errorToNull = false
}
