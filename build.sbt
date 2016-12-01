scalaVersion in ThisBuild := "2.11.8"

name := "nothingAtStakeCoin"
organization := "com.atixlabs"
version := "2.0.0-SNAPSHOT"

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
resolvers += "SonaType snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  "org.scorexfoundation" %% "scorex-core" % "2.0.0-SNAPSHOT",
  "org.scalatest" %% "scalatest" % "2.+" % "test",
  "org.scalactic" %% "scalactic" % "2.+" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.+" % "test",
  "com.typesafe.akka" %% "akka-testkit" % "2.4.14"
)

scalacOptions ++= Seq("-feature", "-deprecation")

javaOptions ++= Seq("-server")

testOptions in Test += Tests.Argument("-oD", "-u", "target/test-reports")