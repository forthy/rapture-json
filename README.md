[![Build Status](https://travis-ci.org/propensive/rapture-json.png?branch=scala-2.10)](https://travis-ci.org/propensive/rapture-json)

# Rapture JSON

Rapture JSON is a comprehensive library providing support for working with JSON in Scala.

### Status

Rapture JSON is *managed*. This means that the API is expected to continue to evolve, but all API changes will be documented with instructions on how to upgrade.

### Availability

Rapture JSON 0.9.0 is available under the Apache 2.0 License from Maven Central with group ID `com.propensive` and artifact ID `rapture-json_2.10`.

#### SBT

You can include Rapture JSON as a dependency in your own project by adding the following library dependency to your build file:

```scala
libraryDependencies ++= Seq("com.propensive" %% "rapture-json" % "0.9.0")
```

#### Maven

If you use Maven, include the following dependency:

```xml
<dependency>
  <groupId>com.propensive</groupId>
  <artifactId>rapture-json_2.10</artifactId>
  <version>0.9.0<version>
</dependency>
```

#### Download

You can download Rapture JSON directly from the [Rapture website](http://rapture.io/)
Rapture JSON depends on Scala 2.10 and Rapture Core, but has no other dependencies.

#### Building from source

To build Rapture JSON from source, follow these steps:

```
git clone git@github.com:propensive/rapture-json.git
cd rapture-json
sbt package
```

If the compilation is successful, the compiled JAR file should be found in target/scala-2.10

