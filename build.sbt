val scala3Version = "3.7.1"

inThisBuild(
  List(
    organization := "ch.epfl.lamp",
    homepage := Some(url("https://lampepfl.github.io/steps")),
    licenses := List(
      "Apache-2.0" -> url(
        s"https://github.com/lampepfl/steps/blob/v${version.value}/LICENSE"
      )
    ),
    developers := List(
      Developer(
        "natsukagami",
        "Natsu Kagami",
        "natsukagami@gmail.com",
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
      "org.scala-lang" %% "scala2-library-cc-tasty-experimental" % scala3Version,
      "org.scalameta" %% "munit" % "0.7.29" % Test
    ),
    scalacOptions ++= Seq(
      // "-Xprint:cc"
    ),
    Compile / doc / scalacOptions ++= Seq(
      "-groups"
    )
  )
