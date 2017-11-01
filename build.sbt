import ReleaseTransformations._

lazy val scala211Version = "2.11.11-bin-typelevel-4"
lazy val scala212Version = "2.12.3-bin-typelevel-4"
lazy val scalametaParadiseVersion = "3.0.0-M10"
lazy val kindProjectorVersion = "0.9.4"
lazy val scalametaVersion = "1.8.0"
lazy val shapelessVersion = "2.3.2"
lazy val catsVersion = "1.0.0-RC1"


lazy val commonSettings: Seq[Def.Setting[_]] = Seq(
  scalaVersion := scala211Version,
  scalaOrganization := "org.typelevel",
  crossScalaVersions := Seq(scala211Version, scala212Version),
  organization := "io.aecor",
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.bintrayIvyRepo("scalameta", "maven")
  ),
  addCompilerPlugin("org.scalameta" % "paradise" % scalametaParadiseVersion cross CrossVersion.patch),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % kindProjectorVersion cross CrossVersion.binary),
  // temporary workaround for https://github.com/scalameta/paradise/issues/10
  scalacOptions in (Compile, console) := Seq(), // macroparadise plugin doesn't work in repl yet.
  // temporary workaround for https://github.com/scalameta/paradise/issues/55
  sources in (Compile, doc) := Nil, // macroparadise doesn't work with scaladoc yet.
  scalacOptions ++= Seq(
    "-Xplugin-require:macroparadise",
    "-Ypartial-unification",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:experimental.macros"
  )
)

lazy val liberator =
  project.in(file(".")).settings(commonSettings).aggregate(macros, tests)


lazy val macros =
  project.settings(
    name := "liberator",
    commonSettings,
    publishSettings,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "scalameta" % scalametaVersion,
      "com.chuusai" %% "shapeless" % shapelessVersion,
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "cats-free" % catsVersion
    )
  )

lazy val tests =
  project
    .settings(
      name := "tests",
      commonSettings,
      noPublishSettings,
      libraryDependencies ++= Seq(
        "org.typelevel" %% "cats-testkit" % catsVersion % Test
      )
    )
    .dependsOn(macros)

lazy val noPublishSettings = Seq(publish := (), publishLocal := (), publishArtifact := false)

lazy val publishSettings = Seq(
  releaseCrossBuild := true,
  releaseCommitMessage := s"Set version to ${if (releaseUseGlobalVersion.value) (version in ThisBuild).value
  else version.value}",
  releaseIgnoreUntrackedFiles := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  homepage := Some(url("https://github.com/aecor/liberator")),
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ =>
    false
  },
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  autoAPIMappings := true,
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/aecor/liberator"),
      "scm:git:git@github.com:aecor/liberator.git"
    )
  ),
  pomExtra :=
    <developers>
      <developer>
        <id>notxcain</id>
        <name>Denis Mikhaylov</name>
        <url>https://github.com/notxcain</url>
      </developer>
    </developers>
)

lazy val sharedReleaseProcess = Seq(
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    publishArtifacts,
    setNextVersion,
    commitNextVersion,
    ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
    pushChanges
  )
)

addCommandAlias("validate", ";compile;test")
