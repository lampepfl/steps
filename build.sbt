val scala3Version = "3.3.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "steps",
    versionScheme := Some("early-semver"),
    scalaVersion := scala3Version,
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    Compile / doc / scalacOptions ++= Seq("-groups"),

    // publish settings
    organization := "ch.epfl.lamp",
    homepage := Some(url("https://lampepfl.github.io/steps")),
    licenses := List(License.Apache2),
    developers := List(
      Developer("hamzaremmal", "Hamza Remmal", "hamza@remmal.dev", url("https://remmal.net")),
      Developer("natsukagami", "Natsu Kagami", "natsukagami@gmail.com", url("https://github.com/natsukagami"))
    )
  )
