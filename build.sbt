import sbt.Keys.resolvers

name := "reify"

version := "0.1"

scalaVersion := "2.12.8"

val derivingVersion = "1.0.0"

val derivingVersionDependencies = Seq(
  // the @deriving and @xderiving plugin and macro
  "org.scalaz" %% "deriving-macro" % derivingVersion,
  compilerPlugin("org.scalaz" %% "deriving-plugin" % derivingVersion),
  // the scalaz-deriving Altz / Decidablez / Deriving API and macros
  "org.scalaz" %% "scalaz-deriving" % derivingVersion,
  // instances for Show and Arbitrary
  "org.scalaz" %% "scalaz-deriving-magnolia" % derivingVersion
)

lazy val reify = (project in file(".")
  settings (resolvers ++= Seq(
    "jcenter" at "http://jcenter.bintray.com"
  ))

  settings(libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
  ) ++ derivingVersionDependencies)
)
