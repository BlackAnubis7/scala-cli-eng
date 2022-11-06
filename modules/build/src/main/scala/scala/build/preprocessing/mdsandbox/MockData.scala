package scala.build.preprocessing.mdsandbox

object MockData {

  val mockMarkdown: (String, Seq[MarkdownSnippet.Fence]) = (
    """# Header 1
      |```scala info1
      |val a: Int = 9; val b: Int = 4
      |```
      |**TEXT**
      |`````md info2 info3
      |/*
      |```scala
      |*/
      |val c: String = "abc" /*
      |```
      |*/
      |````````
      |`Text?`
      |```scala reset
      |var d: Int = 0""".stripMargin,
    List(
      MarkdownSnippet.Fence(List("scala", "info1"), "val a: Int = 9; val b: Int = 4", 2, 2),
      MarkdownSnippet.Fence(List("md", "info2", "info3"), "/*\n```scala\n*/\nval c: String = \"abc\" /*\n```\n*/", 6, 11),
      MarkdownSnippet.Fence(List("scala", "reset"), "var d: Int = 0", 15, 15),
    )
  )
}
