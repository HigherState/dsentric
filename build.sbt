import sbt.Keys._

lazy val buildSettings = Seq(
  organization       := "io.higherState",
  scalaVersion       := "2.11.7",
  version            := "0.1.0",
  scalacOptions     ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:implicitConversions", "-language:higherKinds", "-language:postfixOps", "-language:reflectiveCalls",
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

lazy val reflect = "org.scala-lang" % "scala-reflect" % "2.11.7"
lazy val scalaz = "org.scalaz" %% "scalaz-core" % "7.2.0"
lazy val shapeless = "com.chuusai" %% "shapeless" % "2.2.5"
lazy val monocle = "com.github.julien-truffaut" %%  "monocle-core" % "1.2.0"
lazy val scodec = "org.scodec" %% "scodec-bits" % "1.0.12"

lazy val argo = "io.argonaut" %% "argonaut" % "6.1"
lazy val scalatest = "org.scalatest" %% "scalatest" % "2.2.4"  % "test"


lazy val settings = buildSettings

lazy val dsentric = project.in(file("."))
  .settings(moduleName := "dsentric")
  .settings(settings)
  .aggregate(core, argonaut)
  .dependsOn(core)

lazy val core = project
  .settings(moduleName := "dsentric-core")
  .settings(settings)
  .settings(libraryDependencies := Seq(reflect, scalaz, shapeless, monocle, scalatest))

lazy val argonaut = project
  .settings(moduleName := "dsentric-argonaut")
  .settings(settings)
  .settings(libraryDependencies := Seq(scalaz, shapeless, monocle, scalatest, argo))
  .dependsOn(core)

lazy val amf = project
  .settings(moduleName := "dsentric-amf")
  .settings(settings)
  .settings(libraryDependencies := Seq(scalaz, shapeless, monocle, scalatest, scodec))
  .dependsOn(core)

