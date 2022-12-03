package scala.build.preprocessing.mdsandbox

import scala.util.matching.Regex

object MarkdownConstants {
  val GLOBAL_IGNORE_STATEMENT: String = "<!-- scala-cli-no-compile -->"
  val GLOBAL_IGNORE_STATEMENT_REGEX: Regex = "^<!--(.*\\s+|)scala-cli-no-compile(\\s+.*|)-->".r
}
