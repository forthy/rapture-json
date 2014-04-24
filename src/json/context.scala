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

/** Provides support for JSON literals, in the form json" { } " or json""" { } """.
  * Interpolation is used to substitute variable names into the JSON, and to extract values
  * from a JSON string. */
class JsonStrings[R <: JsonRepresentation](sc: StringContext)(implicit parser: Parser[String, R])
    extends {
  object json {
    /** Creates a new interpolated JSON object. */
    def apply(exprs: ForcedConversion*)(implicit eh: ExceptionHandler): eh.![Json, ParseException] =
      eh.wrap {
        val sb = new StringBuilder
        val textParts = sc.parts.iterator
        val expressions = exprs.iterator
        sb.append(textParts.next())
        while(textParts.hasNext) {
          sb.append(new Json(Array(expressions.next.value))(parser.representation).toString)
          sb.append(textParts.next)
        }
        new Json(Array(parser.parse(sb.toString).get))(parser.representation)
      }

    /** Extracts values in the structure specified from parsed JSON.  Each element in the JSON
      * structure is compared with the JSON to extract from.  Broadly speaking, elements whose
      * values are specified in the extractor must match, whereas variable elements appearing
      * in the extractor must exist. JSON arrays may not appear in the extractor. */
      def unapplySeq(json: Json): Option[Seq[Json]] = try {
        val placeholder = Utils.uniqueNonSubstring(sc.parts.mkString)
        val PlaceholderNumber = (placeholder+"([0-9]+)"+placeholder).r
        val next = new Counter(0)
        val txt = sc.parts.reduceLeft(_ + s""""${placeholder}${next()}${placeholder}" """ + _)
        val paths: Array[Vector[String]] =
          Array.fill[Vector[String]](sc.parts.length - 1)(Vector())
        
        def extract(any: Any, path: Vector[String]): Unit = {
          import strategy.throwExceptions
          if(parser.representation.isNumber(any)) {
            if(json.extract(path).as[Double](doubleExtractor, ?) !=
                parser.representation.getDouble(any)) throw new Exception("Value doesn't match")
          } else if(parser.representation.isString(any)) {
            if(json.extract(path).as[String](stringExtractor, ?) !=
                parser.representation.getString(any)) throw new Exception("Value doesn't match")
          } else if(parser.representation.isBoolean(any)) {
            if(json.extract(path).as[Boolean](booleanExtractor, ?) !=
                parser.representation.getBoolean(any)) throw new Exception("Value doesn't match")
          } else if(parser.representation.isObject(any)) {
            parser.representation.getObject(any) foreach { case (k, v) =>
              if(parser.representation.isString(v)) parser.representation.getString(v) match {
                case PlaceholderNumber(n) =>
                  paths(n.toInt) = path :+ k
                case _ => extract(v, path :+ k)
              } else extract(v, path :+ k)
            }
          } else throw new Exception("Can't match on arrays.")
        }
            

        extract(parser.parse(txt).get, Vector())

        val extracts = paths.map(json.extract)
        if(extracts.exists(_.root(0) == null)) None
        else Some(extracts map { x => new Json(Array(x.normalize))(parser.representation) })
      } catch { case e: Exception => None }
  }
}
