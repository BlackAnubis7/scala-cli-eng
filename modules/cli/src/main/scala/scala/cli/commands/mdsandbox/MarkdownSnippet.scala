package scala.cli.commands.mdsandbox

import scala.collection.mutable

object MarkdownSnippet {
  private val allowIndentedFence: Boolean = false

  case class Fence(
    info: Seq[String],
    body: String,
    startLine: Int,  // start of fenced body EXCLUDING backticks
    endLine: Int     // same as above
  ) {
    override def toString(): String = s"Fence[$info, lines $startLine-$endLine]{${body.replace("\n", "\\n")}}"
    def resetScope: Boolean = false
  }

  private case class StartedFence(
    info: String,
    tickStartLine: Int,  // fence start INCLUDING backticks
    backticks: String,
    indent: Int
  )

  /**
    * Closes started code-fence
    *
    * @param started [[StartedFence]] representing this code-fence's start
    * @param tickEndLine number of the line where closing backticks are
    * @param lines input file sliced into lines
    * @return [[Fence]] representing whole closed code-fence
    */
  private def closeFence(started: StartedFence, tickEndLine: Int, lines: Array[String]): Fence = {
    val start: Int = started.tickStartLine + 1
    val bodyLines: Array[String] = lines.slice(start, tickEndLine)
    Fence(
      started.info.split("\\s+").toList,  // strip info by whitespaces
      bodyLines.tail.foldLeft(bodyLines.head)((body, line) => body.:++("\n"+line)),
      start,  // snippet has to begin in the new line
      tickEndLine - 1  // ending backticks have to be placed below the snippet
    )
  }

  /**
    * Finds all code snippets in given input
    *
    * @param md Markdown file in a `String` format
    * @return list of all found snippets
    */
  def findFences(md: String): Seq[Fence] = {
    var startedFenceOpt: Option[StartedFence] = None
    val fences = mutable.ListBuffer.empty[Fence]
    val lines: Array[String] = md.split("\n\r?")
    for (i <- 0 until lines.length) {
      val line = lines(i)
      startedFenceOpt match {
        case Some(s) => {
          val start: Int = line.indexOf(s.backticks)
          if (start == s.indent && line.forall(c => c == '`' || c.isWhitespace)) {
            fences += closeFence(s, i, lines)
            startedFenceOpt = None
          }
        } case None => {
          val start: Int = line.indexOf("```")
          if (start == 0 || (start > 0 && allowIndentedFence)) {  // doesn't allow snippet indent
            val fence = line.substring(start)
            val backticks: String = fence.takeWhile(_ == '`')
            val info: String = fence.substring(backticks.length)
            startedFenceOpt = Some(StartedFence(info, i, backticks, start))
          }
        }
      }
    }
    startedFenceOpt match {  // snippet can be ended with EOF
      case Some(s) => {
        fences += closeFence(s, lines.length, lines)
        startedFenceOpt = None
      }
      case None =>
    }

    fences.toList
  }
}
