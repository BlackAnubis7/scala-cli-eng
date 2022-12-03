package scala.cli.commands.markdown

import scala.collection.mutable.Map
import scala.build.preprocessing.mdsandbox.MarkdownControlGenerator.Snippet
import scala.build.preprocessing.mdsandbox.MarkdownConstants
import scala.util.matching.Regex
import java.io.{File, FileNotFoundException, IOException, FileWriter}
import java.util.Scanner

private object MarkdownFileWriter {
  private def createOutFile(basePath: String): String = {
    // we suppose it's an ".md" file
    def fname(index: Int): String = s"${basePath.dropRight(3)}_out${index}.md"
    var index: Int = 0
    while (!(new File(fname(index))).createNewFile()) index += 1
    fname(index)
  }

  private def backtickSize(content: String): Int = {
    val reg = "`+".r
    reg.findAllIn(content).toList.sortBy(_.length).lastOption match
      case None => 3
      case Some(longest) => Math.max(3, longest.length() + 1)
  }

  private def wrap(content: String): String = {
    val backticks = "`" * backtickSize(content)
    s"${backticks}text\n${content.stripLineEnd}\n${backticks}"
  }

  private def appendln(writer: FileWriter, csq: CharSequence): Unit = 
    writer.append(s"$csq\n")

  def writeToOutFile(path: String, indexedOutputs: Map[Int, String]): Unit = {
    try {
      val outFilePath = createOutFile(path)
      val outFile = new FileWriter(outFilePath)
      val reader: Scanner = new Scanner(new File(path))
      var lineIndex: Int = 0
      outFile.write(s"${MarkdownConstants.GLOBAL_IGNORE_STATEMENT}\n")
      while (reader.hasNextLine()) {
        if (indexedOutputs.contains(lineIndex)) {
          appendln(outFile, wrap(indexedOutputs(lineIndex)))
        }

        appendln(outFile, reader.nextLine())
        lineIndex += 1
      }
      reader.close()
      if (indexedOutputs.contains(lineIndex)) {
        appendln(outFile, wrap(indexedOutputs(lineIndex)))
      }
      outFile.close()
      println(s"Created output file: ${outFilePath}")
    } catch {
      case e => System.err.println(s"Could not write to file ${path} - ${e}")
    }
  }
}

class MarkdownOutputBuilder {
  private val collected = Map.empty[String, Map[Snippet, String]]
  private val currentContent = new StringBuilder()

  private var currentLocale: Option[(String, Snippet)] = None
  
  private def addCurrentSnippet(): Unit = {
    currentLocale match
      case None => currentContent.clear()
      case Some((file, snippet)) => addSnippet(file, snippet)
  }

  def setLocale(file: Option[String], snippet: Option[Snippet]): Unit = {
    if (file.isDefined || snippet.isDefined) {
      addCurrentSnippet()
      currentLocale = snippet match
        case None => None
        case Some(newSnippet) =>
          file match
            case None => 
              currentLocale match
                case None => None
                case Some((oldFile, oldSnippet)) => Some((oldFile, newSnippet))
            case Some(newFile) => Some((newFile, newSnippet))
    }
  }

  def appendContent(line: String): Unit = {
    currentContent ++= s"${line}\n"
  }

  def addSnippet(file: String, snippet: Snippet): Unit = {
    collected.get(file) match
      case None => 
        collected.addOne((file, Map[Snippet, String]((snippet, currentContent.toString()))))
      case Some(value) =>
        value.addOne((snippet, currentContent.toString()))
    currentContent.clear()
  }

  def createOutFiles(): Unit = {
    addCurrentSnippet()
    collected.foreachEntry{(file, snippetMap) => 
      MarkdownFileWriter.writeToOutFile(file, snippetMap.map((snippet, content) => (snippet.lineEnd + 2, content)))
    }
  }
}
