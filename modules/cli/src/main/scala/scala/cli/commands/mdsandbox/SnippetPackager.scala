package scala.cli.commands.mdsandbox

class SnippetPackager(val fileName: String, val snippets: Seq[MarkdownSnippet.Fence]) {
  val fileIdentifier = fileName.takeWhile(c => c.isLetterOrDigit || c == '_')

  def className(index: Int): String = s"Snippet$index"

  def buildScalaMain(): String = {
    (0 until snippets.length).foldLeft(
      s"object markdown_$fileIdentifier {def main(args: Array[String]): Unit = {"
    ) (
      (sum, index) => sum :++ s"new ${className(index)}; "
    )
    .:++("} ")
    .:++(buildScalaMain(0, 0))
    .:++("}")
  }

  private def buildScalaMain(index: Int, line: Int): String = {
    if (index >= snippets.length) ""
    else {
      val fence: MarkdownSnippet.Fence = snippets(index)
      ("\n" * (fence.startLine - line - 1))  // padding
        .:++(s"class ${className(index)} {\n")
        .:++(fence.body)
        .:++("\n}")
        .:++(buildScalaMain(index + 1, fence.endLine + 1))
    }
  }
}
