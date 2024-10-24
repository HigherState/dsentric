

lazy val buildSettings = Seq(
  organization := "io.higherState",
  scalaVersion := "2.13.14",
  version := "1.3.2",
  scalacOptions ++= (scalaPartV.value match {
    case Some((3, _)) =>
      Seq("-language:postfixOps", "-language:reflectiveCalls", "-language:existentials")
    case _            =>
      Seq(
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
        "-Ywarn-value-discard",
        "-Xsource:3"
      )
  }),
  scalacOptions --= (scalaPartV.value match {
    case Some((3, _)) =>
      Seq("-Ykind-projector")
    case _            =>
      Seq()
  }),
  resolvers ++= Seq(
    DefaultMavenRepository,
    Resolver.typesafeIvyRepo("releases"),
    Resolver.sbtPluginRepo("releases"),
    Resolver.jcenterRepo,
    "Sonatype releases" at "https://oss.sonatype.org/content/repositories/releases/",
    "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
  ),
  (Compile / unmanagedSourceDirectories) ++= {
    (Compile / unmanagedSourceDirectories).value.flatMap { dir =>
      scalaPartV.value match {
        case Some((3, _)) => Seq(new File(dir.getPath + "_3"))
        case _            => Seq(new File(dir.getPath + "_2"))
      }
    }
  },
  (Test / unmanagedSourceDirectories) ++= {
    (Test / unmanagedSourceDirectories).value.flatMap { dir =>
      scalaPartV.value match {
        case Some((3, _)) => Seq(new File(dir.getPath + "_3"))
        case _            => Seq(new File(dir.getPath + "_2"))
      }
    }
  },
  libraryDependencies ++= (scalaPartV.value match {
    case Some((3, _)) =>
      Seq.empty
    case _            =>
      Seq(compilerPlugin("com.github.ghik" % "silencer-plugin" % "1.7.19" cross CrossVersion.full))
  })
)

def scalaPartV =
  Def.setting(CrossVersion.partialVersion(scalaVersion.value))

// sbt-release plugin settings

releaseUseGlobalVersion := false

lazy val reflect      = "org.scala-lang"     % "scala-reflect"  % "2.13.8"
lazy val staging      = "org.scala-lang"    %% "scala3-staging" % "3.3.1"
lazy val shapeless    = "com.chuusai"       %% "shapeless"      % "2.3.3"
lazy val scalatest    = "org.scalatest"     %% "scalatest"      % "3.2.10" % "test"
lazy val cats         = "org.typelevel"     %% "cats-core"      % "2.8.0"
lazy val commons_math = "org.apache.commons" % "commons-math3"  % "3.6.1"
lazy val silencer     = "com.github.ghik"    % "silencer-lib"   % "1.7.19" % Provided cross CrossVersion.full

lazy val settings = buildSettings

lazy val core = project
  .settings(moduleName := "dsentric-core")
  .settings(settings)
  .settings(libraryDependencies ++= (scalaPartV.value match {
    case Some((3, _)) =>
      Seq(reflect, scalatest, commons_math)
    case _            =>
      Seq(reflect, shapeless, scalatest, commons_math)
  }))
  .settings(crossScalaVersions := Seq("2.13.14", "3.5.2"))

lazy val maps = project
  .settings(moduleName := "dsentric-maps")
  .settings(settings)
  .settings(libraryDependencies ++= (scalaPartV.value match {
    case Some((3, _)) =>
      Seq(scalatest, cats, staging)
    case _            =>
      Seq(reflect, shapeless, scalatest, cats, silencer)
  }))
  .dependsOn(core, core % "test -> test")
  .settings(crossScalaVersions := Seq("2.13.14", "3.5.2"))

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
