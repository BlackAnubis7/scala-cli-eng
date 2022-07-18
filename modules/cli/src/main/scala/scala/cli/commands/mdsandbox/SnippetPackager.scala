package scala.cli.commands.mdsandbox

import MarkdownSnippet.Fence

class SnippetPackager(val fileName: String, val snippets: Seq[Fence]) {
  val fileIdentifier = fileName.takeWhile(c => c.isLetterOrDigit || c == '_')

  def className(index: Int): String = s"Snippet$index"

  def buildScalaMain(): String = {
    (0 until snippets.length).foldLeft(
      s"object markdown_$fileIdentifier {def main(args: Array[String]): Unit = {"
    ) (
      (sum, index) => 
        if (snippets(index).resetScope || index == 0) sum :++ s"new ${className(index)}; "
        else sum  // that class hasn't been created
    )
    .:++("} ")
    .:++(buildScalaMain(0, 0))
    .:++("}")
  }

  private def buildScalaMain(index: Int, line: Int): String = {
    if (index >= snippets.length) ""
    else {
      val fence: Fence = snippets(index)
      val classOpener: String =
        if (index == 0)            s"class ${className(index)} {\n"    // first snippet needs to open a class
        else if (fence.resetScope) s"} class ${className(index)} {\n"  // if scope is being reset, close previous class and open a new one
        else "\n"
      ("\n" * (fence.startLine - line - 1))                 // padding
        .:++(classOpener)                                   // new class opening (if applicable)
        .:++(fence.body)                                    // snippet body
        .:++("\n")                                          // padding in place of closing backticks
        .:++(buildScalaMain(index + 1, fence.endLine + 1))  // further snippets
    }
  }
}
