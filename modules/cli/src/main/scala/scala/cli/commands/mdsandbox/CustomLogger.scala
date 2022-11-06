package scala.cli.commands.mdsandbox

import scala.build.Logger
import scala.cli.internal.CliLogger
import java.io.PrintStream

class CustomLogger extends CliLogger(0, false, None, System.out) {
  override def log(message: => String, debugMessage: => String) =
    super.log(s"<<<NEW MESSAGE: $message>>>", debugMessage)

  // val out: PrintStream = System.out

  // def error(message: String): Unit = out.println
  
  // def message(message: => String): Unit 
  // def log(s: => String): Unit
  // def log(s: => String, debug: => String): Unit
  // def debug(s: => String): Unit

  // def log(diagnostics: Seq[Diagnostic]): Unit

  // def log(ex: BuildException): Unit
  // def debug(ex: BuildException): Unit
  // def exit(ex: BuildException): Nothing

  // def coursierLogger(printBefore: String): coursier.cache.CacheLogger
  // def bloopRifleLogger: BloopRifleLogger
  // def scalaJsLogger: ScalaJsLogger
  // def scalaNativeTestLogger: sn.Logger
  // def scalaNativeCliInternalLoggerOptions: List[String]

  // def compilerOutputStream: PrintStream

  // def verbosity: Int
}
