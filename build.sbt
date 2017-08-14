scalaVersion in ThisBuild := "2.12.3"
scalacOptions in ThisBuild ++= Seq(
  "-language:_",
  "-Ypartial-unification",
  "-Xfatal-warnings"
)

// cats 0.9.0
val catsDependencies = Seq(
  "org.typelevel" %% "cats"        % "0.9.0",
  "org.typelevel" %% "cats-effect" % "0.4"
)

// compiler plugins
val compilerPlugins = Seq(
  "org.spire-math" %% "kind-projector" % "0.9.4",
  "org.scalameta"  % "paradise"        % "3.0.0-M10" cross CrossVersion.patch
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
  "co.fs2" %% "fs2-io"   % "0.9.7"
  //"co.fs2" %% "fs2-cats"   % "0.4.0"
)


// unit test
val scalaTestDependencies = Seq(
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

// common settings
val commonSettings = Seq(
  organization := "ufs3",
  resolvers += Resolver.sonatypeRepo("releases")
)

lazy val ufs3 = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= fs2Dependencies,
    libraryDependencies ++= catsDependencies,
    libraryDependencies ++= freestyleDependencies,
    libraryDependencies ++= scalaTestDependencies,
    compilerPlugins.flatMap(addCompilerPlugin)
  )
