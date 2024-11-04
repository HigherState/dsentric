

lazy val buildSettings = Seq(
  organization := "io.higherState",
  scalaVersion := "3.5.2",
  version := "1.3.4",
  scalacOptions ++= Seq("-language:postfixOps", "-language:reflectiveCalls", "-language:existentials"),
  scalacOptions --= Seq("-Ykind-projector"),
  resolvers ++= Seq(
    DefaultMavenRepository,
    Resolver.typesafeIvyRepo("releases"),
    Resolver.sbtPluginRepo("releases"),
    Resolver.jcenterRepo,
    "Sonatype releases" at "https://oss.sonatype.org/content/repositories/releases/",
    "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
  )
)

releaseUseGlobalVersion := false

lazy val reflect      = "org.scala-lang"     % "scala-reflect"  % "2.13.8"
lazy val staging      = "org.scala-lang"    %% "scala3-staging" % "3.3.1"
lazy val shapeless    = "com.chuusai"       %% "shapeless"      % "2.3.3"
lazy val scalatest    = "org.scalatest"     %% "scalatest"      % "3.2.10" % "test"
lazy val cats         = "org.typelevel"     %% "cats-core"      % "2.8.0"
lazy val commons_math = "org.apache.commons" % "commons-math3"  % "3.6.1"

lazy val settings = buildSettings

lazy val core = project
  .settings(moduleName := "dsentric-core")
  .settings(settings)
  .settings(libraryDependencies ++= Seq(reflect, scalatest, commons_math))

lazy val maps = project
  .settings(moduleName := "dsentric-maps")
  .settings(settings)
  .settings(libraryDependencies ++= Seq(scalatest, cats, staging))
  .dependsOn(core, core % "test -> test")


