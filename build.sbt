val scala3Version = "3.3.4"

inThisBuild(List(
  organization := "ch.epfl.lamp",
  homepage := Some(url("https://lampepfl.github.io/steps")),
  licenses := List("Apache-2.0" -> url(s"https://github.com/lampepfl/steps/blob/v${version.value}/LICENSE")),
  developers := List(
    Developer("natsukagami", "Natsu Kagami", "natsukagami@gmail.com", url("https://github.com/natsukagami")),
    Developer("hamzaremmal", "Hamza Remmal", "hamza@remmal.net", url("https://remmal.net")),
    Developer("bracevac", "Oliver Bračevac", "oliver@bracevac.org", url("https://bracevac.org"))
  )
))


lazy val root = project
  .in(file("."))
  .settings(
    name := "steps",
    scalaVersion := scala3Version,
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    Compile / doc / scalacOptions ++= Seq(
      "-groups"
    )
  )
