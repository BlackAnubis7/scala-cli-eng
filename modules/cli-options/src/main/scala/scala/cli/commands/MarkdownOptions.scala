package scala.cli.commands

import caseapp._

// format: off
final case class MarkdownOptions(
  @HelpMessage("Use markdown mode")
  @Name("md")
    markdown: Option[Boolean] = None
)
// format: on

object MarkdownOptions {
  implicit lazy val parser: Parser[VersionOptions] = Parser.derive
  implicit lazy val help: Help[VersionOptions]     = Help.derive
}
