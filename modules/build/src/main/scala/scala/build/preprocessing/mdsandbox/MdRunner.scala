package scala.build.preprocessing.mdsandbox

import scala.build.Inputs
import java.io.EOFException

object MdRunner {
  /**
    * Generates an executor for given `fileName`
    */
  def markdownExecutor(fileName: String): String = s"`Markdown$$${sanitiseIdentifier(fileName)}`.execute()"

  /**
    * Creates a virtual Scala file, which executes snippets from other Markdown files
    *
    * @param inputs inputs given to Scala-CLI by the user
    * @return created virtual Scala file
    */
  def generateRunnerFile(inputs: Inputs): Inputs.VirtualScalaFile = {
    val flatInputs: Seq[Inputs.SingleElement] = inputs.flattened()

    val content: Array[Byte] = flatInputs
      .collect{
        case Inputs.MarkdownFile(base, subPath) => 
          markdownExecutor(subPath.toString)
        case vmf: Inputs.VirtualMarkdownFile => 
          markdownExecutor(vmf.subPath.toString)
      }
      .mkString("object Markdown$Runner {def main(args: Array[String]): Unit = {", "; ", "; println()}}")
      .getBytes()
    val source: String = "markdown_runner.scala"

    Inputs.VirtualScalaFile(content, source)
  }

  def sanitiseIdentifier(ident: String): String =
    ident.foldLeft("")((a: String, b: Char) => a.:+(if (b=='_'||b.isLetterOrDigit) b else '$'))
}
