object project extends ProjectSettings {
  def scalaVersion = "2.10.4"
  def version = "1.0.6"
  def name = "json"
  def description = "Rapture JSON is a comprehensive library providing support for working with JSON in Scala"
  
  def dependencies = Seq(
    "data" -> "1.0.6"
  )
  
  def thirdPartyDependencies = Nil

  def imports = Seq(
    "rapture.core._",
    "rapture.json._",
    "jsonBackends.scalaJson._"
  )
}
