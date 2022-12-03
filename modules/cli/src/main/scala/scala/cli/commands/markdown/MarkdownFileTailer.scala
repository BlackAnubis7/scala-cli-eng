package scala.cli.commands.markdown

import org.apache.commons.io.input.{Tailer, TailerListenerAdapter}
import java.io.File

import scala.build.preprocessing.mdsandbox.MarkdownControlGenerator

private class NeutralFileTailerListener extends TailerListenerAdapter {
  override def handle(line: String): Unit = {
    println(line)
  }
}

private class MarkdownFileTailerListener extends TailerListenerAdapter {
  private var lateFileHeader: Option[String] = None
  private var lateSnippetHeader: Option[String] = None

  private def readableFileHeader(path: String): String =
    s"# File: $path"

  private def readableSnippetHeader(snippet: MarkdownControlGenerator.Snippet): String =
    s"## Snippet #${snippet.index} [${snippet.lineBegin}-${snippet.lineEnd}]"

  override def handle(line: String): Unit = {
    val fileControl = MarkdownControlGenerator.extractFileControlHeader(line)
    val snippetControl = MarkdownControlGenerator.extractSnippetControlHeader(line)

    if (fileControl.isDefined) {
      lateFileHeader = Some(readableFileHeader(fileControl.get))
      lateSnippetHeader = None
    } else {
      if (snippetControl.isDefined) {
        lateSnippetHeader = Some(readableSnippetHeader(snippetControl.get))
      } else if (!line.isBlank()) {
        lateFileHeader match {
          case Some(header) => println(header)
          case None => 
        }
        lateSnippetHeader match {
          case Some(header) => println(header)
          case None => 
        }
        lateFileHeader = None
        lateSnippetHeader = None
        println(line)
      }
    }
  }
}

object MarkdownFileTailer {
  private val delayMillis: Long = 100

  def attachNeutralTailerTo(file: File): Tailer =
    new Tailer(file, new NeutralFileTailerListener, delayMillis)

  def attachMarkdownTailerTo(file: File): Tailer =
    new Tailer(file, new MarkdownFileTailerListener, delayMillis)

  def sleepForDelay() = Thread.sleep((delayMillis * 1.5).toLong)
}
