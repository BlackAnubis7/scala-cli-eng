package scala.build.preprocessing.mdsandbox

import MarkdownSnippet.Fence

class SnippetPackager(val fileName: String, val snippets: Seq[Fence]) {
  def this(fileName: String, content: String) = this(fileName, MarkdownSnippet.findFences(content))

  val (rawSnippets, processedSnippets) = snippets.partition(f => f.isRaw)
  val (testSnippets, runSnippets) = processedSnippets.partition(f => f.isTest)

  val runObjectIdentifier: String = s"`Markdown_$fileName`"
  val testObjectIdentifier: String = s"`Markdown_Test_$fileName`"

  def runClassName(index: Int): String = s"Snippet$index"
  def testClassName(index: Int): String = s"`Test_${fileName}_$index`"

  def buildScalaMain(): String = {
    if (runSnippets.isEmpty) s"object $runObjectIdentifier {}"  // TODO - no snippets handling
    else (0 until runSnippets.length).foldLeft(
      s"object $runObjectIdentifier {def main(args: Array[String]): Unit = {"
    ) (
      (sum, index) => 
        if (runSnippets(index).resetScope || index == 0) sum :++ s"new ${runClassName(index)}; "
        else sum  // that class hasn't been created
    )
    .:++("} ")
    .:++(buildScalaMain(0, 0))
    .:++("}")
  }

  private def buildScalaMain(index: Int, line: Int): String = {
    if (index >= runSnippets.length) "}"  // close last class
    else {
      val fence: Fence = runSnippets(index)
      val classOpener: String =
        if (index == 0)            s"class ${runClassName(index)} {\n"     // first snippet needs to open a class
        else if (fence.resetScope) s"}; class ${runClassName(index)} {\n"  // if scope is being reset, close previous class and open a new one
        else "\n"
      ("\n" * (fence.startLine - line - 1))                 // padding
      // ("\n" * (1))                 // no padding
        .:++(classOpener)                                   // new class opening (if applicable)
        .:++(fence.body)                                    // snippet body
        .:++("\n")                                          // padding in place of closing backticks
        .:++(buildScalaMain(index + 1, fence.endLine + 1))  // further snippets
    }
  }

  /** Returns test snippets packed into classes and glued together */
  def buildScalaTest(): String = {
    if (testSnippets.isEmpty) s"object $testObjectIdentifier {}"  // TODO - no snippets handling
    else buildScalaTest(0, 0, Seq()) :++ "}"
  }

  private def buildScalaTest(index: Int, line: Int, imported: Seq[String]): String = {
    if (index >= testSnippets.length) ""
    else {
      val fence: Fence = testSnippets(index)
      // val newImports: Seq[String] = fence.modifiers
      //   .getOrElse("import", "")
      //   .split("[,;+]")
      //   .filter(i => i.nonEmpty && !imported.contains(i))
      //   .toIndexedSeq
      // val importString: String = 
      //   if (newImports.isEmpty) ""
      //   else newImports.foldLeft("")((sum, imp) => sum :++ s"import $imp;")
      val usingLib: String = fence.modifiers.get("lib") match {
        case Some(lib) => s"/*> using lib \"${lib}\" */"
        case None      => ""
      }
      val extensions: Option[String] = fence.modifiers.get("extends")
      val extendsClause: String = if (extensions.isDefined) s" extends ${extensions.get}" else ""
      val classOpener: String =
        if (index == 0)  s"class ${testClassName(index)}$extendsClause {\n"    // first snippet needs to open a class
        else             s"}; class ${testClassName(index)}$extendsClause {\n"  // if scope is being reset, close previous class and open a new one
      ("\n" * (fence.startLine - line - 1))                 // padding
        // .:++("/*> using lib \"org.scalameta::munit::0.7.29\" */" /*:++ importString*/)                                  // imports
        .:++(usingLib)
        .:++(classOpener)                                   // new class opening
        .:++(fence.body)                                    // snippet body
        .:++("\n")                                          // padding in place of closing backticks
        .:++(buildScalaTest(index + 1, fence.endLine + 1, imported/* ++ newImports*/))  // further test snippets
    }
  }

  def buildScalaRaw(): String = {
    if (rawSnippets.isEmpty) ""
    else buildScalaRaw(0, 0)
  }

  private def buildScalaRaw(index: Int, line: Int): String = {
    if (index >= rawSnippets.length) ""
    else {
      val fence: Fence = rawSnippets(index)
      ("\n" * (fence.startLine - line))  // padding
        .:++(fence.body)                     // snippet body
        .:++("\n")                           // padding in place of closing backticks
        .:++(buildScalaRaw(index + 1, fence.endLine + 1))
    }
  }
}
