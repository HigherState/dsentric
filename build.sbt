import sbt.Keys._

lazy val buildSettings = Seq(
  organization       := "io.higherState",
  scalaVersion       := "2.11.8",
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

lazy val reflect = "org.scala-lang" % "scala-reflect" % "2.11.8"
lazy val scalaz = "org.scalaz" %% "scalaz-core" % "7.2.0"
lazy val shapeless = "com.chuusai" %% "shapeless" % "2.2.5"
lazy val monoclecore = "com.github.julien-truffaut" %%  "monocle-core" % "1.2.0"
lazy val scodec = "org.scodec" %% "scodec-bits" % "1.0.12"
lazy val argo = "io.argonaut" %% "argonaut" % "6.1"
lazy val scalatest = "org.scalatest" %% "scalatest" % "2.2.4"  % "test"


lazy val settings = buildSettings

//lazy val dsentric = project.in(file("."))
//  .settings(moduleName := "dsentric")
//  .settings(settings)
//  .aggregate(core, monocle)
//  .dependsOn(core)

lazy val core = project
  .settings(moduleName := "dsentric-core")
  .settings(settings)
  .settings(libraryDependencies := Seq(reflect, shapeless, scalatest))

lazy val monocle = project
  .settings(moduleName := "dsentric-monocle")
  .settings(settings)
  .settings(libraryDependencies := Seq(reflect, scalaz, shapeless, monoclecore, scalatest))
  .dependsOn(core)

lazy val maps = project
  .settings(moduleName := "dsentric-maps")
  .settings(settings)
  .settings(libraryDependencies := Seq(reflect, shapeless, scalatest))
  .dependsOn(core)

lazy val argonaut = project
  .settings(moduleName := "dsentric-argonaut")
  .settings(settings)
  .settings(libraryDependencies := Seq(scalatest, argo))
  .dependsOn(monocle)

//lazy val amf = project
//  .settings(moduleName := "dsentric-amf")
//  .settings(settings)
//  .settings(libraryDependencies := Seq(monocle, scalatest, scodec))
//  .dependsOn(core)
//
lazy val performance = project
  .settings(moduleName := "dsentric-performance")
  .settings(settings)
  .settings(libraryDependencies := Seq(scalatest))
  .dependsOn(core, argonaut, maps)

