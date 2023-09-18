import sbt.Keys._

lazy val buildSettings = Seq(
  organization := "io.higherState",
  scalaVersion := "2.13.8",
  version := "1.1.6",
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:postfixOps",
    "-language:reflectiveCalls",
    "-language:existentials",
    "-unchecked",
    "-Xfatal-warnings",
    "-Ywarn-dead-code",
    "-Ywarn-value-discard"
  ),
  addCompilerPlugin("com.github.ghik" % "silencer-plugin" % "1.7.11" cross CrossVersion.full),
  resolvers ++= Seq(
    DefaultMavenRepository,
    Resolver.typesafeIvyRepo("releases"),
    Resolver.sbtPluginRepo("releases"),
    Resolver.jcenterRepo,
    "Sonatype releases" at "https://oss.sonatype.org/content/repositories/releases/",
    "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
  )
)
// sbt-release plugin settings

releaseUseGlobalVersion := false

// Replace 'publish' step with 'publishLocal' step
releaseProcess -= ReleaseTransformations.publishArtifacts

// end sbt-release plugin settings

// enable publishing the jar produced by `test:package`
Test / packageBin / publishArtifact := true

// enable publishing the test API jar
Test / packageDoc / publishArtifact := true

// enable publishing the test sources jar
Test / packageSrc / publishArtifact := true
publishTo in ThisBuild := Some(
  "Artifactory Realm" at "https://artifactory.orgvue.com:443/artifactory/company-sbt-release"
)

lazy val reflect      = "org.scala-lang"     % "scala-reflect" % "2.13.8"
lazy val shapeless    = "com.chuusai"       %% "shapeless"     % "2.3.3"
lazy val scalatest    = "org.scalatest"     %% "scalatest"     % "3.2.7"  % "test"
lazy val cats         = "org.typelevel"     %% "cats-core"     % "2.8.0"
lazy val commons_math = "org.apache.commons" % "commons-math3" % "3.6.1"
lazy val silencer     = "com.github.ghik"    % "silencer-lib"  % "1.7.11" % Provided cross CrossVersion.full

lazy val settings = buildSettings

lazy val core = project
  .settings(moduleName := "dsentric-core")
  .settings(settings)
  .settings(libraryDependencies ++= Seq(reflect, shapeless, scalatest, commons_math))

lazy val maps   = project
  .settings(moduleName := "dsentric-maps")
  .settings(settings)
  .settings(libraryDependencies ++= Seq(reflect, shapeless, scalatest, cats, silencer))
  .dependsOn(core, core % "test -> test")

lazy val macros = project
  .settings(moduleName := "dsentric-macros")
  .settings(settings)
  .settings(
    scalacOptions ++= Seq(
      "-Ymacro-annotations",
      "-Xmacro-settings:materialize-derivations",
      "-Ywarn-macros:after",
      "-Ywarn-unused:explicits",
      "-language:reflectiveCalls"
    )
  )
  .settings(libraryDependencies ++= Seq(reflect, shapeless, scalatest, commons_math))
  .dependsOn(core, maps)
