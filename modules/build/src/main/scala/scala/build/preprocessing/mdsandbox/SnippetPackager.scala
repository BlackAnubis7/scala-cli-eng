package scala.build.preprocessing.mdsandbox

import MarkdownSnippet.Fence
import MdRunner.sanitiseIdentifier
import scala.runtime.IntRef

class SnippetPackager(val filePath: String, val snippets: Seq[Fence]) {
  def this(filePath: String, content: String) = this(filePath, MarkdownSnippet.findFences(content))

  val (rawSnippets, processedSnippets) = snippets.partition(f => f.isRaw)
  val (testSnippets, runSnippets) = processedSnippets.partition(f => f.isTest)

  val runObjectIdentifier: String = s"Markdown$$${sanitiseIdentifier(filePath)}"
  val testObjectIdentifier: String = s"`Markdown_Test$$${sanitiseIdentifier(filePath)}`"

  /** Generates class name for a snippet with given index */
  private def runClassName(index: Int): String = s"Snippet$$$index"

  /** Generates class name for a test snippet with given index */
  private def testClassName(index: Int): String = s"`Test$$${sanitiseIdentifier(filePath)}_$index`"

  private val fileControlPrint = s"println(\"${MarkdownControlGenerator.fileControlHeader(filePath)}\"); "
  private def snippetControlPrint(fence: Fence): String = 
    s"println(\"${MarkdownControlGenerator.snippetControlHeader(fence.index, fence.startLine, fence.endLine)}\"); "

  /** Returns Scala snippets packed into classes and glued together into an object */
  def buildScalaMain(): String = {
    if (runSnippets.isEmpty) s"object $runObjectIdentifier {def execute(): Unit = {}}"  // no snippets
    else (0 until runSnippets.length).foldLeft(
      s"object $runObjectIdentifier {def execute(): Unit = {${fileControlPrint}"
    ) (
      (sum, index) => {
        val fence = runSnippets(index)
        if (
          fence.resetScope 
          || index == 0 
          || (index > 0 && runSnippets(index - 1).isGlobal && !fence.isGlobal)
          ) sum :++ s"new ${runClassName(fence.index)}; "
        else sum  // that class hasn't been created
      }
    )
    .:++("}; ")
    .:++(buildScalaMain(0, 0))
    .:++("}")
  }

  private def buildScalaMain(index: Int, line: Int, isClassOpened: Boolean = false): String = {
    if (isClassOpened && index >= runSnippets.length) "}"  // close last class, if opened
    else {
      val fence: Fence = runSnippets(index)
      val classOpener: String =
        if (fence.isGlobal)
          if (isClassOpened) "}\n"  // global snippet, some class opened already
          else "\n"
        else if (!isClassOpened) s"class ${runClassName(fence.index)} {${snippetControlPrint(fence)}\n"     // no class currently opened
        else if (fence.resetScope) s"}; class ${runClassName(fence.index)} {${snippetControlPrint(fence)}\n"  // if scope is being reset, close previous class and open a new one
        else s"${snippetControlPrint(fence)}\n"
      val tryOpener: String =
        if (fence.isFail) "try {"
        else ""
      val closingPadding: String =  // padding in place of closing backticks
        if (fence.isFail) "} catch {case e => println(s\"Error occurred - ${e.toString()}\")}\n"
        else "\n"
      ("\n" * (fence.startLine - line - 1))                 // padding
        .:++(classOpener)                                   // new class opening (if applicable)
        .:++(tryOpener)                                     // try / catch opening (if applicable)
        .:++(fence.body)                                    // snippet body
        .:++(closingPadding)                                // try / catch ending (if applicable)    
        .:++(buildScalaMain(index + 1, fence.endLine + 1, !fence.isGlobal))  // further snippets
    }
  }

  /** Returns test snippets packed into classes and glued together into an object */
  def buildScalaTest(): String = {
    if (testSnippets.isEmpty) s"object $testObjectIdentifier {}"  // no snippets
    else buildScalaTest(0, 0) :++ "}"
  }

  /*
   * TESTING APPROACH A - RAW TESTING
   * ```scala test
   */
  // private def buildScalaTest(index: Int, line: Int): String = {
  //   if (index >= testSnippets.length) ""
  //   else {
  //     val fence: Fence = testSnippets(index)
  //     ("\n" * (fence.startLine - line))      // padding
  //       .:++(fence.body)                     // snippet body
  //       .:++("\n")                           // padding in place of closing backticks
  //       .:++(buildScalaTest(index + 1, fence.endLine + 1))
  //   }
  // }

  /*
   * TESTING APPROACH B - EXPLICITLY DEFINED FRAMEWORK
   * ```scala test lib=org.scalameta::munit::0.7.29 extends=munit.FunSuite
   */
  private def buildScalaTest(index: Int, line: Int): String = {
    if (index >= testSnippets.length) ""
    else {
      val fence: Fence = testSnippets(index)

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
        .:++(usingLib)
        .:++(classOpener)                                   // new class opening
        .:++(fence.body)                                    // snippet body
        .:++("\n")                                          // padding in place of closing backticks
        .:++(buildScalaTest(index + 1, fence.endLine + 1))  // further test snippets
    }
  }

  /*
   * TESTING APPROACH C - HARDCODED FRAMEWORK LIST
   * ```scala test framework=munit
   */
  // private def buildScalaTest(index: Int, line: Int): String = {
  //   if (index >= testSnippets.length) ""
  //   else {
  //     val fence: Fence = testSnippets(index)
  //     val testFrameworks: Map[String, (String, String)] = Map(
  //       "munit" -> ("org.scalameta::munit::0.7.29", "munit.FunSuite")
  //     )
  //     val (usingLib, extendsClause): (String, String) =
  //       fence.modifiers.get("framework") match {
  //         case Some(fw) => testFrameworks.get(fw) match
  //           case Some((lib, ext)) => (s"/*> using lib \"${lib}\" */", s" extends ${ext}")
  //           case None       => ("", "")
  //         case None     => ("", "")
  //       }

  //     val classOpener: String =
  //       if (index == 0)  s"class ${testClassName(index)}$extendsClause {\n"     // first snippet needs to open a class
  //       else             s"}; class ${testClassName(index)}$extendsClause {\n"  // if scope is being reset, close previous class and open a new one
  //     ("\n" * (fence.startLine - line - 1))                 // padding
  //       .:++(usingLib)
  //       .:++(classOpener)                                   // new class opening
  //       .:++(fence.body)                                    // snippet body
  //       .:++("\n")                                          // padding in place of closing backticks
  //       .:++(buildScalaTest(index + 1, fence.endLine + 1))  // further test snippets
  //   }
  // }

  /** Returns raw snippets glued together into one file */
  def buildScalaRaw(): String = {
    if (rawSnippets.isEmpty) ""
    else buildScalaRaw(0, 0)
  }

  private def buildScalaRaw(index: Int, line: Int): String = {
    if (index >= rawSnippets.length) ""
    else {
      val fence: Fence = rawSnippets(index)
      ("\n" * (fence.startLine - line))      // padding
        .:++(fence.body)                     // snippet body
        .:++("\n")                           // padding in place of closing backticks
        .:++(buildScalaRaw(index + 1, fence.endLine + 1))
    }
  }
}
