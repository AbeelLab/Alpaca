name := "Alpaca"

version := "0.1"

scalaVersion := "2.12.4"

resolvers += Resolver.bintrayRepo("underscoreio", "training")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "com.github.scopt" % "scopt_2.12" % "3.7.0",
  "com.sksamuel.avro4s" %% "avro4s-core" % "1.9.0",
  "underscoreio" %% "doodle" % "0.8.2")

unmanagedJars in Compile ++= (file("lib/") *  "*.jar").classpath
