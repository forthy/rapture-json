object project extends ProjectSettings {
  def scalaVersion = "2.10.4"
  def version = "0.9.1"
  def name = "json"
  def description = "Rapture JSON is a comprehensive library providing support for working with JSON in Scala"
  
  def dependencies = Seq(
    "core" -> "0.9.0"
  )
  
  def thirdPartyDependencies = Nil

  def imports = Seq(
    "rapture.core._",
    "rapture.json._",
    "jsonParsers.scalaJson._",
    "strategy.throwExceptions"
  )
}
