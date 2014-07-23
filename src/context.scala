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

/** Provides support for JSON literals, in the form json" { } " or json""" { } """.
  * Interpolation is used to substitute variable names into the JSON, and to extract values
  * from a JSON string. */
class JsonStrings(sc: StringContext)(implicit parser: JsonParser[String])
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
          sb.append(new Json(Array(expressions.next.value)).toString)
          sb.append(textParts.next)
        }
        Json.parse(sb.toString)(parser, raw)
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
        val paths: Array[Vector[Either[Int, String]]] =
          Array.fill[Vector[Either[Int, String]]](sc.parts.length - 1)(Vector())
        
        def extract(any: Any, path: Vector[Either[Int, String]]): Unit = {
          import strategy.throwExceptions
          if(parser.isNumber(any)) {
            if(json.extract(path).as[Double](Extractor.doubleExtractor, ?) !=
                parser.getDouble(any)) throw new Exception("Value doesn't match")
          } else if(parser.isString(any)) {
            if(json.extract(path).as[String](Extractor.stringExtractor, ?) !=
                parser.getString(any)) throw new Exception("Value doesn't match")
          } else if(parser.isBoolean(any)) {
            if(json.extract(path).as[Boolean](Extractor.booleanExtractor, ?) !=
                parser.getBoolean(any)) throw new Exception("Value doesn't match")
          } else if(parser.isObject(any)) {
            parser.getObject(any) foreach { case (k, v) =>
              if(parser.isString(v)) parser.getString(v) match {
                case PlaceholderNumber(n) =>
                  paths(n.toInt) = path :+ Right(k)
                case _ => extract(v, path :+ Right(k))
              } else extract(v, path :+ Right(k))
            }
          } else {
            parser.getArray(any).zipWithIndex foreach { case (e, i) =>
              if(parser.isString(e)) parser.getString(e) match {
                case PlaceholderNumber(n) =>
                  paths(n.toInt) = path :+ Left(i)
                case _ => extract(e, path :+ Left(i))
              } else extract(e, path :+ Left(i))
            }
          }
        }
            
        extract(parser.parse(txt).get, Vector())

        val extracts = paths.map(json.extract)
        if(extracts.exists(_.root(0) == null)) None
        else Some(extracts map { x => new Json(Array(x.normalize)) })
      } catch { case e: Exception => None }
  }
}
