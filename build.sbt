val scala3Version = "3.8.4"
resolvers += ("Artifactory" at "https://repo.scala-lang.org/artifactory/maven-nightlies/")

inThisBuild(
  List(
    organization := "ch.epfl.lamp",
    homepage := Some(url("https://lampepfl.github.io/steps")),
    versionScheme := Some("early-semver"),
    licenses := List(
      License(
        "Apache-2.0",
        url(s"https://github.com/lampepfl/steps/blob/v${version.value}/LICENSE")
      )
    ),
    developers := List(
      Developer(
        "natsukagami",
        "Natsu Kagami",
        "nki@fastmail.com",
        url("https://github.com/natsukagami")
      ),
      Developer(
        "hamzaremmal",
        "Hamza Remmal",
        "hamza@remmal.net",
        url("https://remmal.net")
      ),
      Developer(
        "bracevac",
        "Oliver Bračevac",
        "oliver@bracevac.org",
        url("https://bracevac.org")
      ),
      Developer(
        "bishabosha",
        "Jamie Thompson",
        "thompsonjamesrichard@gmail.com",
        url("https://bishabosha.github.io")
      )
    )
  )
)

lazy val root = project
  .in(file("."))
  .aggregate(steps.projectRefs *)
  .settings(
    publish / skip := true,
    compile / skip := true
  )

lazy val steps = (projectMatrix in file("."))
  .settings(
    name := "steps",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      // "org.scala-lang" %% "scala2-library-cc-tasty-experimental" % scala3Version,
      "org.scalameta" %% "munit" % "1.3.0" % Test
    ),
    scalacOptions ++= Seq(
      // "-Xprint:cc"
      "-preview",
      "-Yexplicit-nulls"
    ),
    Compile / doc / scalacOptions ++= Seq(
      "-groups"
    )
  )
  .jvmPlatform(scalaVersions = Seq(scala3Version))
  .jsPlatform(
    scalaVersions = Seq(scala3Version),
    settings = Seq(
      scalaJSUseMainModuleInitializer := false
    )
  )
  .nativePlatform(
    scalaVersions = Seq(scala3Version),
    settings = Seq(
      nativeConfig ~= {
        _.withIncrementalCompilation(true)
      },
      // The Scala Native 0.5.12 toolchain pulls test-interface 0.5.12, while
      // munit 1.3.0 still pins 0.5.11. Selecting 0.5.12 is correct (it matches
      // the toolchain). sbt 2.x promotes such version conflicts to a hard error
      // by default; keep the sbt 1.x behaviour of warning instead.
      evictionErrorLevel := Level.Warn
    )
  )
