addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.6.1")
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.11.2")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.22.0")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.12")
// sbt 2.x in-sources cross-platform project matrices (`projectMatrix`), so the
// portable-scala crossproject plugins are no longer needed (and aren't published
// for sbt 2.x).
// sbt-ci-release bundles an older JGit that does not support git worktrees.
// Overriding to JGit 7+ fixes the NoWorkTreeException on project load.
// Note: JGit 7+ requires Java 17+ (already the sbt 2.x minimum).
libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "7.0.0.202409031743-r"
