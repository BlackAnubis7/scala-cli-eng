package scala.build.preprocessing.mdsandbox

import scala.util.Try

object MarkdownControlGenerator {
  case class Snippet(
    index: Int,
    lineBegin: Int,
    lineEnd: Int
  )

  private val controlPrefix = "}#}#}#"
  private val controlSuffix = controlPrefix

  private val fileControlRegex = 
    s"${controlPrefix}FILE@(?<path>.+)%$controlSuffix\\n?\\r?".r
  private val snippetControlRegex = 
    s"${controlPrefix}SNIPPET@(?<index>\\d+)%S@(?<lineBegin>\\d+)%E@(?<lineEnd>\\d+)%$controlSuffix\\n?\\r?".r

  def fileControlHeader(absPath: String): String =
    s"${controlPrefix}FILE@$absPath%$controlSuffix"

  def snippetControlHeader(index: Int, lineBegin: Int, lineEnd: Int): String =
    s"${controlPrefix}SNIPPET@$index%S@$lineBegin%E@$lineEnd%$controlSuffix"

  def extractFileControlHeader(header: String): Option[String] =
    fileControlRegex.findFirstMatchIn(header) match
      case None => None
      case Some(m) => Some(m.group("path"))

  def extractSnippetControlHeader(header: String): Option[Snippet] =
    snippetControlRegex.findFirstMatchIn(header) match
      case None => None
      case Some(m) => Try(Some(Snippet(
        m.group("index").toInt,
        m.group("lineBegin").toInt,
        m.group("lineEnd").toInt
      ))).getOrElse(None)
}
