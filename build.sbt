name := "cats"

version := "0.1"

scalaVersion := "2.13.10"

val catsVersion = "2.9.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
)

scalacOptions ++= Seq(
  "-language:higherKinds"
)