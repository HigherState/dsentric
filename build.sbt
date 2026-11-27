lazy val buildSettings = Seq(
  organization := "io.higherState",
  scalaVersion := "3.10.1",
  version      := "1.4.1",
  scalacOptions ++= Seq("-source:3.6", "-language:postfixOps", "-language:reflectiveCalls", "-language:existentials"),
  scalacOptions --= Seq("-Ykind-projector"),
  scalacOptions ~= (_.map { case "-Xfatal-warnings" => "-Werror"; case option => option }),
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

lazy val staging      = "org.scala-lang"    %% "scala3-staging"  % "3.10.1"
lazy val scalatest    = "org.scalatest"     %% "scalatest"       % "3.2.20" % "test"
lazy val cats         = "org.typelevel"     %% "cats-core"       % "2.13.0"
lazy val commons_math = "org.apache.commons" % "commons-math3"   % "3.6.1"
lazy val shapeless3   = "com.orgvue"        %% "shapeless3-core" % "0.1.1"

lazy val settings = buildSettings

lazy val core = project
  .settings(moduleName := "dsentric-core")
  .settings(settings)
  .settings(libraryDependencies ++= Seq(scalatest, commons_math))

lazy val maps = project
  .settings(moduleName := "dsentric-maps")
  .settings(settings)
  .settings(libraryDependencies ++= Seq(scalatest, cats, staging, shapeless3))
  .dependsOn(core, core % "test -> test")

lazy val dsentric = (project in file("."))
  .aggregate(core, maps)
  .settings(settings)
  .settings(publish / skip := true)
