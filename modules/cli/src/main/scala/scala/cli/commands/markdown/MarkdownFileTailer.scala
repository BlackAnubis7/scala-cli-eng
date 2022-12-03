package scala.cli.commands.markdown

import org.apache.commons.io.input.{Tailer, TailerListenerAdapter}
import java.io.File
import scala.build.preprocessing.mdsandbox.MarkdownControlGenerator

private class NeutralFileTailerListener extends TailerListenerAdapter {
  override def handle(line: String): Unit = {
    println(line)
  }
}

private class MarkdownFileTailerListener(
  val contentOnlyPrinter: String => Unit,
  val readablePrinter: String => Unit,
  val outputCollector: MarkdownOutputBuilder
) extends TailerListenerAdapter {
  private var lateFileHeader: Option[String] = None
  private var lateSnippetHeader: Option[MarkdownControlGenerator.Snippet] = None

  private def printHeader(header: String): Unit = {
    readablePrinter(header)
  }

  private def printContent(content: String): Unit = {
    outputCollector.appendContent(content)
    readablePrinter(content)
    contentOnlyPrinter(content)
  }

  private def readableFileHeader(path: String): String =
    s"# File: $path"

  private def readableSnippetHeader(snippet: MarkdownControlGenerator.Snippet): String =
    s"## Snippet #${snippet.index + 1} [lines ${snippet.lineBegin + 1}-${snippet.lineEnd + 1}]"

  override def handle(line: String): Unit = {
    val fileControl = MarkdownControlGenerator.extractFileControlHeader(line)
    val snippetControl = MarkdownControlGenerator.extractSnippetControlHeader(line)

    if (fileControl.isDefined) {
      lateFileHeader = fileControl
      lateSnippetHeader = None
    } else {
      if (snippetControl.isDefined) {
        lateSnippetHeader = snippetControl
      } else if (!line.isBlank()) {
        lateFileHeader match {
          case Some(header) => printHeader(readableFileHeader(header))
          case None => 
        }
        lateSnippetHeader match {
          case Some(header: MarkdownControlGenerator.Snippet) => printHeader(readableSnippetHeader(header))
          case None => 
        }
        outputCollector.setLocale(lateFileHeader, lateSnippetHeader)
        lateFileHeader = None
        lateSnippetHeader = None
        printContent(line)
      }
    }
  }
}

object MarkdownFileTailer {
  private val delayMillis: Long = 100

  def attachNeutralTailerTo(file: File): Tailer =
    new Tailer(file, new NeutralFileTailerListener, delayMillis)

  def attachMarkdownTailerTo(
    file: File,
    contentOnlyPrinter: String => Unit,
    readablePrinter: String => Unit,
    outputCollector: MarkdownOutputBuilder
  ): Tailer =
    new Tailer(file, new MarkdownFileTailerListener(contentOnlyPrinter, readablePrinter, outputCollector), delayMillis)

  def sleepForDelay() = Thread.sleep((delayMillis * 1.5).toLong)
}
