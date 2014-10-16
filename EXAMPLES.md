# Rapture JSON Quickstart

## Imports

Code                                  | Description
--------------------------------------|-----------------------------------------------------------------
`import rapture.core._`               | Access core Rapture functionality, like Modes
`import modes.returnTry`              | Have all fallible methods return results wrapped in `Try`
`import modes.captureExceptions`      | Have all fallible methods return results wrapped in `Either`
`import rapture.json._`               | Use the Rapture JSON library
`import jsonBackends.scalaJson._`     | Use the Scala standard library JSON backend
`import jsonBackends.argonaut._`      | Use the Argonaut backend
`import jsonBackends.jackson._`       | Use the Jackson backend
`import jsonBackends.jawn._`          | Use the Jawn backend
`import jsonBackends.json4s._`        | Use the JSON4S backend
`import jsonBackends.lift._`          | Use the Lift JSON backend
`import jsonBackends.spray._`         | Use the Spray JSON backend
`import patternMatching.exactObjects` | Do not match objects with spurious additional keys
`import patternMatching.exactArrays`  | Do not match arrays with spurious additional elements
`import patternMatching.exact`        | Exact matching of both arrays and objects
`import formatters.compact`           | Output JSON to `String`s without whitespace
`import formatters.humanReadable`     | Output JSON to `String`s formatted with newlines and indentation


## Parsing JSON

Code                       | Description
---------------------------|----------------------------------------
`Json.parse(src)`          | Parse the string `src`
`json"""{"foo":42}"""`     | A JSON "literal"
`json"""{ "foo" : $j }"""` | A JSON "literal" with substituted value

## Accessing values

Code                      | Description
--------------------------|-------------------------------------------------
`json.candidates`         | Access the `candidates` value of the JSON object
`json.candidates(0)`      | Access the first array of the `candidates` array
`json.candidates(0).name` | Access the `name` value of the first candidate

## Extracting values

Code                    | Description
------------------------|----------------------------------------------------------------------------------------------------------------------
`json.as[String]`       | Extract a `String` from the JSON value
`json.as[Int]`          | Extract an `Int` from the JSON value
`json.as[Double]`       | Extract a `Double` from the JSON value
`json.as[Boolean]`      | Extract a `Boolean` from the JSON value
`json.as[List[String]]` | Extracts a `List` of `String`s from a JSON array
`json.as[Set[String]]`  | Extracts a `Set` of `String`s from a JSON array
`json.as[Vector[Int]]`  | Extracts a `Vector` of `Int`s from a JSON array
`json.as[MyCaseClass]`  | Extracts an instance of `MyCaseClass` from a JSON object (assumes every parameter of the case class can be extracted)
`json.as[Option[Int]]`  | Extract `Some[Int]` if the value exists, otherwise extract `None`

## Converting to JSON

Code                                   | Description
---------------------------------------|-----------------------------------------------------------------------------------
`Json(7)`                              | Returns a `Json` integer
`Json("Hello World!")`                 | Returns a `Json` string
`Json(true)`                           | Returns a `Json` boolean
`Json(List(1, 2, 3))`                  | Returns a `Json` array
`Json(Set("red", "green", "blue"))`    | Returns a `Json` array
`Json(MyCaseClass(42, "Hello", true))` | Returns a `Json` object
`Json(backendJsonValue)`               | Returns a Rapture `Json` value which wraps a type from the underlying JSON backend

*To be completed*
