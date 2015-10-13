import sbt.Keys._

lazy val buildSettings = Seq(
  organization       := "io.higherState",
  scalaVersion       := "2.11.7",
  scalacOptions     ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:implicitConversions", "-language:higherKinds", "-language:postfixOps",
    "-unchecked",
    "-Xfatal-warnings",
    "-Yinline-warnings",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-value-discard",
    "-Xfuture"
  ),
  resolvers ++= Seq(
    "Maven Central Server" at "http://repo1.maven.org/maven2",
    "Sonatype releases" at "http://oss.sonatype.org/content/repositories/releases/",
    "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
  )
)

lazy val scalaz = "org.scalaz" %% "scalaz-core" % "7.1.4"
lazy val shapeless = "com.chuusai" %% "shapeless" % "2.2.5"
lazy val monocle = "com.github.julien-truffaut" %%  "monocle-core" % "1.2.0-M1"
lazy val scalatest = "org.scalatest" %% "scalatest" % "2.2.4"  % "test"


lazy val settings = buildSettings

lazy val dsentric = project.in(file("."))
  .settings(moduleName := "dsentric")
  .settings(settings)
  .aggregate(core)
  .dependsOn(core)

lazy val core = project
  .settings(moduleName := "dsentric-core")
  .settings(settings)
  .settings(libraryDependencies := Seq(scalaz, shapeless, monocle, scalatest))