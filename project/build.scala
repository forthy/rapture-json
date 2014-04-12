/**********************************************************************************************\
* Rapture Core Library                                                                         *
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

import sbt._
import Keys._

trait ProjectSettings {
  def scalaVersion: String
  def version: String
  def name: String
  def description: String
  def dependencies: Seq[(String, String)]
  def thirdPartyDependencies: Seq[(String, String, String)]
  def imports: Seq[String]
  def mainClass: String = null
}

object RaptureBuild extends Build {

  def proj: ProjectSettings = project

  lazy val root = Project(s"rapture-${proj.name}", file("."))

  override val settings = super.settings ++ Seq(
    name := s"rapture-${proj.name}",
    organization := "com.propensive",
    description := proj.description,
    version in ThisBuild := proj.version,
    scalaVersion := proj.scalaVersion,
    homepage := Some(url("http://rapture.io")),
    licenses += ("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
    scalacOptions ++= Seq(
      "-Yinline-warnings",
      "-deprecation",
      "-optimize",
      "-unchecked",
      "-encoding", "UTF-8",
      "-feature",
      "-target:jvm-1.6"
    ),
    pomExtra := (
      <scm>
        <url>git@github.com:propensive/rapture-{proj.name}.git</url>
        <connection>scm:git:git@github.com:propensive/rapture-{proj.name}.git</connection>
      </scm>
      <developers>
        <developer>
          <id>propensive</id>
          <name>Jon Pretty</name>
          <url>http://rapture.io/</url>
        </developer>
      </developers>
    ),
    publishTo := Some("releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2"),
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    ),
    publishMavenStyle := true,
    libraryDependencies ++= proj.dependencies.map { case (p, v) =>
      "com.propensive" %% s"rapture-$p" % v
    },
    libraryDependencies ++= proj.thirdPartyDependencies.map { case (g, p, v) => g % p % v },
    initialCommands in console := proj.imports.map("import "+_).mkString("\n"),
    packageOptions in (Compile, packageBin) += Package.ManifestAttributes(java.util.jar.Attributes.Name.MAIN_CLASS -> proj.mainClass)
  )

}
