package steps.testing

import scala.language.unsafeNulls

object TestDriver extends dotty.tools.dotc.Driver {
  private def classLoaderUrls(cl: ClassLoader): List[java.net.URL] = cl match
    case null => Nil
    case cl: java.net.URLClassLoader => cl.getURLs.toList ::: classLoaderUrls(cl.getParent)
    case cl => classLoaderUrls(cl.getParent)

  private val classpath: String =
    val entries = classLoaderUrls(getClass.getClassLoader)
      .map(url => java.nio.file.Paths.get(url.toURI).toString)
    (sys.props("java.class.path") :: entries).mkString(java.io.File.pathSeparator)

  val args = Array("-classpath", classpath, "_.scala", "-Ystop-after:erasure")

  def runWithErrors(src: String): Seq[String] =
    val reporter = new dotty.tools.dotc.reporting.StoreReporter()
    val rootCtx = initCtx.fresh.setReporter(reporter)
    setup(args, rootCtx) match
      case Some((_, compileCtx)) =>
        val src0 = new dotty.tools.io.VirtualFile(
          "<src>.scala",
          src.getBytes(java.nio.charset.StandardCharsets.UTF_8)
        )
        doCompile(newCompiler(using compileCtx), List(src0))(using compileCtx)
      case None =>
        ???
    reporter.allErrors.map(_.message)
}
