package scala.cli.commands

import caseapp._

// format: off
final case class MarkdownOptions(
  @HelpMessage("Compile and run markdown scala snippets")
  @Name("md")
    markdown: Option[Boolean] = None,

  @HelpMessage("Use only markdown \"Raw\" snippets. Gets overriden by --markdown")
  @Name("mdraw")
    markdown_raw: Option[Boolean] = None,

  @HelpMessage("Generate output markdown files. Works only when passed together with --markdown")
  @Name("mdout")
    generate_markdown_output: Option[Boolean] = None,
)
// format: on

object MarkdownOptions {
  implicit lazy val parser: Parser[VersionOptions] = Parser.derive
  implicit lazy val help: Help[VersionOptions]     = Help.derive
}
