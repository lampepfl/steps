addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.6")
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.11.2")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.21.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
// sbt-ci-release bundles an older JGit that does not support git worktrees.
// Overriding to JGit 7+ fixes the NoWorkTreeException on project load.
// Note: JGit 7+ requires Java 17+.
libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "7.0.0.202409031743-r"