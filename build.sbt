val scala3Version = "3.8.1"
resolvers += ("Artifactory" at "https://repo.scala-lang.org/artifactory/maven-nightlies/")

inThisBuild(
  List(
    organization := "ch.epfl.lamp",
    homepage := Some(url("https://lampepfl.github.io/steps")),
    versionScheme := Some("early-semver"),
    licenses := List(
      "Apache-2.0" -> url(
        s"https://github.com/lampepfl/steps/blob/v${version.value}/LICENSE"
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
  .settings(
    name := "steps",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      // "org.scala-lang" %% "scala2-library-cc-tasty-experimental" % scala3Version,
      "org.scalameta" %% "munit" % "0.7.29" % Test
    ),
    scalacOptions ++= Seq(
      // "-Xprint:cc"
      "-preview"
    ),
    Compile / doc / scalacOptions ++= Seq(
      "-groups"
    )
  )
