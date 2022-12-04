package scala.cli.commands.markdown

import scala.build.Inputs
import scala.cli.commands.MarkdownOptions
import scala.build.options.{BuildOptions, MarkdownBuildOptions}
import scala.build.Inputs.Script
import scala.build.Inputs.VirtualScript
import scala.build.Inputs.ScalaFile
import scala.build.Inputs.JavaFile
import scala.build.Inputs.MarkdownFile
import scala.build.Inputs.Directory
import scala.build.Inputs.ResourceDirectory
import scala.build.Inputs.VirtualData
import scala.build.Inputs.VirtualJavaFile
import scala.build.Inputs.VirtualScalaFile
import scala.build.Inputs.VirtualMarkdownFile

object MarkdownDataGenerator {
  def generateMarkdownInputs(inputs: Inputs, __md: Boolean, includeRunner: Boolean): Inputs = {
    if (__md) {
      if (includeRunner) {
        inputs.copy(
          elements = inputs.elements :+ scala.build.preprocessing.mdsandbox.MdRunner.generateRunnerFile(inputs)
        )
      } else inputs
    } else inputs.copy(
      elements = inputs.flattened().filter{
        _ match
          case MarkdownFile(_, _) => false
          case VirtualMarkdownFile(_, _) => false
          case _ => true
      }
    )
  }

  def generateMarkdownInputsForRun(inputs: Inputs, mdOptions: MarkdownOptions): Inputs = {
    val mdMode: Boolean = mdOptions.markdown == Some(true) || mdOptions.markdown_raw == Some(true)
    val includeRunner: Boolean = mdOptions.markdown == Some(true)
    generateMarkdownInputs(inputs, mdMode, includeRunner)
  }

  def generateMarkdownInputsForTest(inputs: Inputs, mdMode: Boolean): Inputs = {
    generateMarkdownInputs(inputs, mdMode, false)
  }

  def generateMarkdownBuildOptions(initialBuildOptions: BuildOptions, fullMdMode: Boolean, rawMdMode: Boolean): BuildOptions = {
    if (fullMdMode) initialBuildOptions.copy(
      mainClass = Some("Markdown$Runner"),
      markdownOptions = mdOptionsCliToBuild(fullMdMode, rawMdMode)
    ) else if (rawMdMode) initialBuildOptions.copy(
      markdownOptions = mdOptionsCliToBuild(fullMdMode, rawMdMode)
    ) else initialBuildOptions
  }

  def generateMarkdownBuildOptionsForRun(initialBuildOptions: BuildOptions, mdOptions: MarkdownOptions): BuildOptions = {
    val fullMdMode: Boolean = mdOptions.markdown == Some(true)
    val rawMdMode: Boolean = mdOptions.markdown_raw == Some(true)
    generateMarkdownBuildOptions(initialBuildOptions, fullMdMode, rawMdMode)
  }

  def generateMarkdownBuildOptionsForTest(initialBuildOptions: BuildOptions, mdMode: Boolean): BuildOptions = {
    generateMarkdownBuildOptions(initialBuildOptions, false, mdMode)
  }

  private def mdOptionsCliToBuild(fullMdMode: Boolean, rawMdMode: Boolean): MarkdownBuildOptions = 
    MarkdownBuildOptions(
      markdown = Some(fullMdMode),
      markdown_raw = Some(rawMdMode)
    )
}
