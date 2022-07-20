package scala.build.preprocessing.mdsandbox

object MakeshiftTests {
  def assert[T](predicate: Boolean)(ifTrue: T, ifFalse: T): T =
    if (predicate) ifTrue else ifFalse

  def printTest[T](testName: String, expected: T, actual: T): Unit =
    println(
      (if (expected == actual) "\u001b[0;32m" else "\u001b[0;31m") 
      + testName 
      + assert(expected == actual)(": OK", s": ${expected} expected, ${actual} received") 
      + "\u001b[0m"
    )

  def testMarkdownSnippet(): Unit = {  // makeshift test - passed
    printTest(
      "MarkdownSnippet.findFences",
      MockData.mockMarkdown._2,
      MarkdownSnippet.findFences(MockData.mockMarkdown._1)
    )
  }

  def main(args: Array[String]): Unit = {
    testMarkdownSnippet()
    println(
      new SnippetPackager("my_favourite.md", MarkdownSnippet.findFences(MockData.mockMarkdown._1)).buildScalaMain()
    )
  }
}
