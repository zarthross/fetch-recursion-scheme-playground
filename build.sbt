val Http4sVersion = "0.20.11"


organization := "test"
name := "test"
version := "0.0.1-SNAPSHOT"
scalaVersion := "2.12.10"
libraryDependencies ++= Seq(
  "io.higherkindness" %% "droste-core" % "0.7.0",
  "com.47deg" %% "fetch" % "1.2.0",
  "org.http4s" %% "http4s-blaze-server" % Http4sVersion,
  "org.http4s" %% "http4s-dsl" % Http4sVersion,
  "org.slf4j" % "slf4j-simple" % "1.7.28"
)
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0")

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-language:higherKinds",
  "-language:postfixOps",
  "-feature",
  "-Ypartial-unification",
  "-Xfatal-warnings"
)
