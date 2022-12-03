package scala.cli.commands.markdown

import scala.build.Inputs
import scala.cli.commands.MarkdownOptions
import scala.build.options.{BuildOptions, MarkdownBuildOptions}

object MarkdownDataGenerator {
  def generateMarkdownInputs(inputs: Inputs, mdOptions: MarkdownOptions): Inputs = {
    val mdMode: Boolean = mdOptions.markdown == Some(true)
    if (mdMode) inputs.copy(
      elements = inputs.elements :+ scala.build.preprocessing.mdsandbox.MdRunner.generateRunnerFile(inputs)
    ) else inputs
  }

  def generateMarkdownBuildOptions(initialBuildOptions: BuildOptions, mdOptions: MarkdownOptions): BuildOptions = {
    val fullMdMode: Boolean = mdOptions.markdown == Some(true)
    val rawMdMode: Boolean = mdOptions.markdown_raw == Some(true)
    if (fullMdMode) initialBuildOptions.copy(
      mainClass = Some("Markdown$Runner"),
      markdownOptions = mdOptionsCliToBuild(mdOptions)
    ) else if (rawMdMode) initialBuildOptions.copy(
      markdownOptions = mdOptionsCliToBuild(mdOptions)
    ) else initialBuildOptions
  }

  private def mdOptionsCliToBuild(mdOptions: MarkdownOptions): MarkdownBuildOptions = 
    MarkdownBuildOptions(
      markdown = mdOptions.markdown,
      markdown_raw = mdOptions.markdown_raw
    )
}
