object project extends ProjectSettings {
  def scalaVersion = "2.11.2"
  def version = "1.0.0"
  def name = "json"
  def description = "Rapture JSON is a comprehensive library providing support for working with JSON in Scala"
  
  def dependencies = Seq(
    "data" -> "1.0.0"
  )
  
  def thirdPartyDependencies = Seq(
    ("org.scala-lang.modules", "scala-parser-combinators_2.11", "1.0.2")
  )

  def imports = Seq(
    "rapture.core._",
    "rapture.json._",
    "jsonBackends.scalaJson._"
  )
}
