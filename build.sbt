name := "types-a-la-carte"

organization := "org.lolczak"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.6"

resolvers += "Tim Tennant's repo" at "http://dl.bintray.com/timt/repo/"

resolvers += "bintray/non" at "http://dl.bintray.com/non/maven"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.5.4")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.2" % "test" withSources() withJavadoc(),
  "org.scalacheck" %% "scalacheck" % "1.11.6" % "test" withSources() withJavadoc(),
  "com.chuusai" %% "shapeless" % "2.1.0" withSources() withJavadoc(),
  "org.scalaz" %% "scalaz-core" % "7.1.2" withSources() withJavadoc()
)