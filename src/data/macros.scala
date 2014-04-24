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

import scala.reflect.macros._
import scala.annotation._

import language.experimental.macros
import language.higherKinds

object Macros {
 
  def extractorMacro[T: c.WeakTypeTag, Data: c.WeakTypeTag](c: Context): c.Expr[Extractor[T, Data]] = {
    import c.universe._

    require(weakTypeOf[T].typeSymbol.asClass.isCaseClass)

    val extractor = typeOf[Extractor[_, _]].typeSymbol.asType.toTypeConstructor

    // FIXME: This will perform badly for large objects, as the map extraction is applied to
    // all elements once for every element
    val params = weakTypeOf[T].declarations collect {
      case m: MethodSymbol if m.isCaseAccessor => m.asMethod
    } map { p =>

      val paramValue = c.Expr[Any](Apply(
        Select(
          Ident(newTermName("data")),
          newTermName("$accessInnerMap")
        ),
        List(Literal(Constant(p.name.toString)))
      ))

      val newArray = reify(Array(paramValue.splice))
      
      val newDataArray = Apply(
        Apply(
          Select(
            New(
              TypeTree(weakTypeOf[Data])
            ),
            nme.CONSTRUCTOR
          ),
          List(newArray.tree)
        ),
        List(Select(
          Ident(newTermName("data")),
          newTermName("representation")
        ))
      )

      Apply(
        Select(
          c.Expr[Extractor[_, _]](
            c.inferImplicitValue(appliedType(extractor, List(p.returnType, weakTypeOf[Data])), false, false)
          ).tree,
          newTermName("construct")
        ),
        List(newDataArray)
      )
    }

    val construction = c.Expr[T](
      Apply(
        Select(
          New(
            TypeTree(weakTypeOf[T])
          ),
          nme.CONSTRUCTOR
        ),
        params.to[List]
      )
    )

    reify(new Extractor[T, Data] { def construct(data: Data): T = construction.splice })
  }

  def serializerMacro[T: c.WeakTypeTag](c: Context)(representation: c.Expr[DataRepresentation]): c.Expr[Serializer[T]] = {
    import c.universe._

    val tpe = weakTypeOf[T].typeSymbol.asClass
    val serializer = typeOf[Serializer[_]].typeSymbol.asType.toTypeConstructor

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
                c.inferImplicitValue(appliedType(serializer, List(p.returnType)), false, false),
                newTermName("serialize")
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

      c.Expr[Map[String, Any]](
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
      c.Expr[Map[String, Any]](
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
                  c.inferImplicitValue(appliedType(serializer, List(sc.asType.toType)), false, false),
                  newTermName("serialize")
                ),
                List(Ident(newTermName("v")))
              )
            )
          }
        )
      )
    } else throw new Exception()

    reify(new Serializer[T] { def serialize(t: T): Any = representation.splice.fromObject(construction.splice) })
  }
}
