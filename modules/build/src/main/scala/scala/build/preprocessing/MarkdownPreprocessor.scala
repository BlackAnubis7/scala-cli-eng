package scala.build.preprocessing

import java.nio.charset.StandardCharsets

import scala.build.EitherCps.{either, value}
import scala.build.errors.BuildException
import scala.build.internal.{AmmUtil, CodeWrapper, CustomCodeWrapper, Name}
import scala.build.options.{BuildOptions, BuildRequirements}
import scala.build.preprocessing.ScalaPreprocessor.ProcessingOutput
import scala.build.{Inputs, Logger}

import scala.build.preprocessing.mdsandbox.SnippetPackager

final case class MarkdownPreprocessor() extends Preprocessor {
  def preprocess(
    input: Inputs.SingleElement,
    logger: Logger
  ): Option[Either[BuildException, Seq[PreprocessedSource]]] =
    input match {
      case markdown: Inputs.MarkdownFile =>
        val res = either {
          val content = value(PreprocessingUtil.maybeRead(markdown.path))
          val preprocessed = value {
            MarkdownPreprocessor.preprocess(
              Right(markdown.path),
              content,
              // codeWrapper,
              markdown.subPath,
              ScopePath.fromPath(markdown.path),
              logger
            )
          }
          preprocessed
        }
        Some(res)

      // case script: Inputs.VirtualScript =>
      //   val content = new String(script.content, StandardCharsets.UTF_8)

      //   val res = either {
      //     val preprocessed = value {
      //       MarkdownPreprocessor.preprocess(
      //         Left(script.source),
      //         content,
      //         codeWrapper,
      //         script.wrapperPath,
      //         script.scopePath,
      //         logger
      //       )
      //     }
      //     preprocessed
      //   }
      //   Some(res)

      case _ =>
        None
    }
}

object MarkdownPreprocessor {

  private def preprocess(
    reportingPath: Either[String, os.Path],
    content: String,
    // codeWrapper: CodeWrapper,
    subPath: os.SubPath,
    scopePath: ScopePath,
    logger: Logger
  ): Either[BuildException, List[PreprocessedSource.InMemory]] = either {

    // val (contentIgnoredSheBangLines, _) = SheBang.ignoreSheBangLines(content)

    // val (pkg, wrapper) = AmmUtil.pathToPackageWrapper(subPath)

    val packager: SnippetPackager = new SnippetPackager(subPath.last, content)
    val parsedMain: String = packager.buildScalaMain()
    val parsedTest: String = packager.buildScalaTest()

    val mainProcessingOutput =
      value(ScalaPreprocessor.process(
        parsedMain,
        reportingPath,
        scopePath / os.up,
        logger
      ))
        .getOrElse(ProcessingOutput(BuildRequirements(), Nil, BuildOptions(), None))

    val testProcessingOutput =
      value(ScalaPreprocessor.process(
        parsedTest,
        reportingPath,
        scopePath / os.up,
        logger
      ))
        .getOrElse(ProcessingOutput(BuildRequirements(), Nil, BuildOptions(), None))

    // val (code, topWrapperLen, _) = codeWrapper.wrapCode(
    //   pkg,
    //   wrapper,
    //   processingOutput.updatedContent.getOrElse(contentIgnoredSheBangLines)
    // )

    val mainCode = mainProcessingOutput.updatedContent.getOrElse(parsedMain)
    val testCode = testProcessingOutput.updatedContent.getOrElse(parsedTest)

    System.out.println(s"<<< MAIN\n${mainCode}\nMAIN >>>")
    System.out.println(s"<<< TEST\n${testCode}\nTEST >>>")
    val topWrapperLen = 0

    // val className = (pkg :+ wrapper).map(_.raw).mkString(".")
    val mainClassName = s"Markdown_${subPath.last}"
    val mainRelPath   = os.rel / (subPath / os.up) / s"${subPath.last.stripSuffix(".md")}.scala"

    // val testClassName = s"Markdown_${subPath.last}"
    val testRelPath   = os.rel / (subPath / os.up) / s"${subPath.last.stripSuffix(".md")}_test.scala"

    val mainFile = PreprocessedSource.InMemory(
      reportingPath.map((subPath, _)),
      mainRelPath,
      mainCode,
      topWrapperLen,
      Some(mainProcessingOutput.opts),
      Some(mainProcessingOutput.globalReqs),
      mainProcessingOutput.scopedReqs,
      Some(CustomCodeWrapper.mainClassObject(Name(mainClassName)).backticked),
      scopePath
    )

    val testFile = PreprocessedSource.InMemory(
      reportingPath.map((subPath, _)),
      testRelPath,
      testCode,
      topWrapperLen,
      Some(testProcessingOutput.opts),
      Some(testProcessingOutput.globalReqs),
      testProcessingOutput.scopedReqs,
      None,
      scopePath
    )

    List(mainFile, testFile)
  }

}
