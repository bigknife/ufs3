import sbt.Keys._
import sbtassembly.AssemblyPlugin._
import sbtassembly.AssemblyPlugin.autoImport._

//moodify your orgnization and version
lazy val commonSettings = Seq(
  scalaVersion in ThisBuild := "2.11.8",
  organization := "ufs3",
  scalacOptions ++= Seq(
    "-target:jvm-1.8",
    "-encoding",
    "UTF-8",
    "-unchecked",
    "-deprecation",
    "-Xfuture",
    "-feature",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Ywarn-unused"
  ),
  resolvers += Resolver.sonatypeRepo("releases"),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
)

lazy val kernel = (project in file("kernel"))
  .settings(commonSettings: _*)
  .settings(
    //add other settings
    libraryDependencies += "org.typelevel" %% "cats"        % "0.9.0",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "0.3",
    libraryDependencies += "org.scalactic" %% "scalactic"   % "3.0.1",
    libraryDependencies += "org.scalatest" %% "scalatest"   % "3.0.1" % "test"
  )

lazy val core = (project in file("core"))
  .dependsOn(kernel)
  .settings(commonSettings: _*)

lazy val interpreter = (project in file("interpreter"))
  .dependsOn(kernel)
  .settings(commonSettings: _*)
  .settings(
    resolvers += "BarcsysRepo" at "https://repox.barcsys.com/",
    libraryDependencies += "log4j"             % "log4j"                   % "1.2.17",
    libraryDependencies += "org.reactivemongo" %% "reactivemongo"          % "0.11.14",
    libraryDependencies += "io.spray"          % "spray-json_2.11"         % "1.3.3",
    libraryDependencies += "com.barcsys"       %% "barcsys_tcp_connection" % "1.0.10",
    libraryDependencies += "org.scalactic"     %% "scalactic"              % "3.0.1",
    libraryDependencies += "org.scalatest"     %% "scalatest"              % "3.0.1" % "test"
  )

lazy val integration = (project in file("integration"))
  .dependsOn(core)
  .dependsOn(interpreter)
  .settings(commonSettings: _*)
.settings(
  libraryDependencies += "com.github.scopt" %% "scopt" % "3.6.0",

  assemblyOption in assembly := (assemblyOption in assembly).value.copy(prependShellScript = Some(defaultShellScript)),
  mainClass in assembly := Some("ufs3.integration.Main"),
  assemblyJarName := s"ufs3"
)
