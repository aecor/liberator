import ReleaseTransformations._

lazy val commonSettings: Seq[Def.Setting[_]] = Seq(
  scalaVersion := "2.11.8",
  scalaOrganization := "org.typelevel",
  crossScalaVersions := Seq("2.11.8", "2.12.1"),
  organization := "io.aecor",
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.bintrayIvyRepo("scalameta", "maven")
  ),
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M7" cross CrossVersion.full),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
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
      "org.scalameta" %% "scalameta" % "1.6.0",
      "com.chuusai" %% "shapeless" % "2.3.2",
      "org.typelevel" %% "cats" % "0.9.0"
    )
  )

lazy val tests =
  project
    .settings(
      name := "tests",
      commonSettings,
      noPublishSettings,
      libraryDependencies ++= Seq(
        "io.monix" %% "monix-eval" % "2.2.1",
        "io.monix" %% "monix-cats" % "2.2.1",
        "io.circe" %% "circe-core" % "0.7.0",
        "io.circe" %% "circe-generic" % "0.7.0",
        "org.scalatest" %% "scalatest" % "3.0.1" % Test,
        "org.scalactic" % "scalactic_2.11" % "3.0.1" % Test
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
