import sbtassembly.AssemblyPlugin._
import sbtassembly.AssemblyPlugin.autoImport._

scalaVersion in ThisBuild := "2.11.8"
scalacOptions in ThisBuild ++= Seq(
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
)

scalacOptions in (Compile, console) ~= (_ filterNot (_ contains "paradise"))

// cats 0.9.0
val catsDependencies = Seq(
  "org.typelevel" %% "cats"        % "0.9.0",
  "org.typelevel" %% "cats-effect" % "0.3"
)

// compiler plugins
val compilerPlugins = Seq(
  "org.spire-math" %% "kind-projector" % "0.9.4",
  "org.scalameta"  % "paradise"        % "3.0.0-M7" cross CrossVersion.patch
)

// freestyle 0.3.1
val freestyleDependencies = Seq(
  "io.frees" %% "freestyle"         % "0.3.1",
  "io.frees" %% "freestyle-effects" % "0.3.1",
  "io.frees" %% "freestyle-cache"   % "0.3.1",
  "io.frees" %% "freestyle-fs2"     % "0.3.1"
)

// fs2

val fs2Dependencies = Seq(
  "co.fs2" %% "fs2-core" % "0.9.7",
  "co.fs2" %% "fs2-io"   % "0.9.7",
  "co.fs2" %% "fs2-cats" % "0.3.0"
)

// unit test
val scalaTestDependencies = Seq(
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

// misc dependencies
val miscDependencies = Seq(
  "com.github.scopt" %% "scopt" % "3.6.0"
)

// common settings
val commonSettings = Seq(
  organization := "ufs3",
  resolvers += Resolver.sonatypeRepo("releases")
)

// assembly settings
val assemblySettings = Seq(
  test in assembly := {},
  assemblyOption in assembly := (assemblyOption in assembly).value
    .copy(prependShellScript = Some(defaultShellScript)),
  mainClass in assembly := Some("ufs3.world.Entrance"),
  assemblyJarName := s"ufs3",
  assemblyMergeStrategy in assembly := {
    case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
    case PathList("META-INF", xs @ _*) => MergeStrategy.concat
    case PathList("application.conf") ⇒ MergeStrategy.concat
    case PathList("reference.conf") ⇒ MergeStrategy.concat
    //case PathList("scala", xs@_*) ⇒ MergeStrategy.discard
    case x ⇒ MergeStrategy.first
  }
)

// barcsys settings
val barcsysSettings = Seq(
  resolvers += "BarcsysRepo" at "https://repox.barcsys.com/",
  libraryDependencies += "com.barcsys" %% "barcsys_tcp_connection" % "1.0.11.20170731180222-SNAPSHOT",
  libraryDependencies += "pharaoh"     %% "pharaoh"                % "0.0.1"
)

lazy val ufs3 = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= fs2Dependencies,
    libraryDependencies ++= catsDependencies,
    libraryDependencies ++= freestyleDependencies,
    libraryDependencies ++= scalaTestDependencies,
    libraryDependencies ++= miscDependencies,
    compilerPlugins.flatMap(addCompilerPlugin)
  )
  .settings(assemblySettings: _*)
  .settings(barcsysSettings)
