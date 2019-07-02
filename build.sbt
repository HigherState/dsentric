import sbt.Keys._

lazy val buildSettings = Seq(
  organization       := "io.higherState",
  scalaVersion       := "2.12.7",
  version            := "0.7.1",
  scalacOptions     ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:implicitConversions", "-language:higherKinds", "-language:postfixOps", "-language:reflectiveCalls",
    "-unchecked",
    "-Xfatal-warnings",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-value-discard",
    "-Xfuture"
  ),
  resolvers ++= Seq(
    DefaultMavenRepository,
    Resolver.typesafeIvyRepo("releases"),
    Resolver.sbtPluginRepo("releases"),
    Resolver.jcenterRepo,
    "Sonatype releases" at "http://oss.sonatype.org/content/repositories/releases/",
    "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
  )
)

lazy val reflect = "org.scala-lang" % "scala-reflect" % "2.12.7"
lazy val shapeless = "com.chuusai" %% "shapeless" % "2.3.3"
lazy val scalatest = "org.scalatest" %% "scalatest" % "3.0.5"  % "test"
lazy val cats = "org.typelevel" %% "cats-core" % "1.4.0"
lazy val commons_math = "org.apache.commons" % "commons-math3" % "3.6.1"


lazy val settings = buildSettings

//lazy val dsentric = project.in(file("."))
//  .settings(moduleName := "dsentric")
//  .settings(settings)
//  .aggregate(core, monocle)
//  .dependsOn(core)

lazy val core = project
  .settings(moduleName := "dsentric-core")
  .settings(settings)
  .settings(libraryDependencies ++= Seq(reflect, shapeless, scalatest, commons_math))

//lazy val monocle = project
//  .settings(moduleName := "dsentric-monocle")
//  .settings(settings)
//  .settings(libraryDependencies := Seq(reflect, scalaz, shapeless, monoclecore, scalatest))
//  .dependsOn(core)

lazy val maps = project
  .settings(moduleName := "dsentric-maps")
  .settings(settings)
  .settings(libraryDependencies ++= Seq(reflect, shapeless, scalatest, cats))
  .dependsOn(core, core % "test -> test")

lazy val graphql = project
  .settings(moduleName := "dsentric-graphql")
  .settings(settings)
  .settings(libraryDependencies ++= Seq(reflect, shapeless, scalatest, commons_math))
  .dependsOn(core, maps, core % "test -> test")

//lazy val argonaut = project
//  .settings(moduleName := "dsentric-argonaut")
//  .settings(settings)
//  .settings(libraryDependencies := Seq(scalatest, argo))
//  .dependsOn(monocle)
