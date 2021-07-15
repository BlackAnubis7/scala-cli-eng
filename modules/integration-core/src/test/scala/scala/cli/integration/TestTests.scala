package scala.cli.integration

import com.eed3si9n.expecty.Expecty.expect

class TestTests extends munit.FunSuite {

  val successfulTestInputs = TestInputs(
    Seq(
      os.rel / "MyTests.scala" ->
        """import $ivy.`org.scalameta::munit::0.7.25`
          |import $ivy.`org.scala-lang:scala-reflect:2.12.13` // seems munit lacks this dependency on the JVM
          |
          |class MyTests extends munit.FunSuite {
          |  test("foo") {
          |    assert(2 + 2 == 4)
          |    println("Hello from " + "tests")
          |  }
          |}
          |""".stripMargin
    )
  )

  val failingTestInputs = TestInputs(
    Seq(
      os.rel / "MyTests.scala" ->
        """import $ivy.`org.scalameta::munit::0.7.25`
          |import $ivy.`org.scala-lang:scala-reflect:2.12.13` // seems munit lacks this dependency
          |
          |class MyTests extends munit.FunSuite {
          |  test("foo") {
          |    assert(2 + 2 == 5, "Hello from " + "tests")
          |  }
          |}
          |""".stripMargin
    )
  )

  val successfulUtestInputs = TestInputs(
    Seq(
      os.rel / "MyTests.scala" ->
        """import $ivy.`com.lihaoyi::utest::0.7.9`, utest._
          |
          |object MyTests extends TestSuite {
          |  val tests = Tests {
          |    test("foo") {
          |      assert(2 + 2 == 4)
          |      println("Hello from " + "tests")
          |    }
          |  }
          |}
          |""".stripMargin
    )
  )

  val successfulUtestJsInputs = TestInputs(
    Seq(
      os.rel / "MyTests.scala" ->
        """import $ivy.`com.lihaoyi::utest::0.7.9`, utest._
          |import scala.scalajs.js
          |
          |object MyTests extends TestSuite {
          |  val tests = Tests {
          |    test("foo") {
          |      assert(2 + 2 == 4)
          |      val console = js.Dynamic.global.console
          |      console.log("Hello from " + "tests")
          |    }
          |  }
          |}
          |""".stripMargin
    )
  )

  val successfulUtestNativeInputs = TestInputs(
    Seq(
      os.rel / "MyTests.scala" ->
        """import $ivy.`com.lihaoyi::utest::0.7.9`, utest._
          |import scala.scalanative.libc._
          |import scala.scalanative.unsafe._
          |
          |object MyTests extends TestSuite {
          |  val tests = Tests {
          |    test("foo") {
          |      assert(2 + 2 == 4)
          |      Zone { implicit z =>
          |        stdio.printf(toCString("Hello from " + "tests\n"))
          |      }
          |    }
          |  }
          |}
          |""".stripMargin
    )
  )

  val successfulJunitInputs = TestInputs(
    Seq(
      os.rel / "MyTests.scala" ->
        """import $ivy.`com.novocode:junit-interface:0.11`
          |import org.junit.Test
          |
          |class MyTests {
          |
          |  @Test
          |  def foo() {
          |    assert(2 + 2 == 4)
          |    println("Hello from " + "tests")
          |  }
          |}
          |""".stripMargin
    )
  )

  val severalTestsInputs = TestInputs(
    Seq(
      os.rel / "MyTests.scala" ->
        """import $ivy.`org.scalameta::munit::0.7.25`
          |import $ivy.`org.scala-lang:scala-reflect:2.12.13` // seems munit lacks this dependency on the JVM
          |
          |class MyTests extends munit.FunSuite {
          |  test("foo") {
          |    assert(2 + 2 == 4)
          |    println("Hello from " + "tests1")
          |  }
          |}
          |""".stripMargin,
      os.rel / "OtherTests.scala" ->
        """import $ivy.`org.scalameta::munit::0.7.25`
          |import $ivy.`org.scala-lang:scala-reflect:2.12.13` // seems munit lacks this dependency on the JVM
          |
          |class OtherTests extends munit.FunSuite {
          |  test("bar") {
          |    assert(1 + 1 == 2)
          |    println("Hello from " + "tests2")
          |  }
          |}
          |""".stripMargin
    )
  )

  test("successful test") {
    successfulTestInputs.fromRoot { root =>
      val output = os.proc(TestUtil.cli, "test", TestUtil.extraOptions, ".").call(cwd = root).out.text
      expect(output.contains("Hello from tests"))
    }
  }

  test("current directory as default input") {
    successfulTestInputs.fromRoot { root =>
      val output = os.proc(TestUtil.cli, "test", TestUtil.extraOptions).call(cwd = root).out.text
      expect(output.contains("Hello from tests"))
    }
  }

  def successfulJsTest(): Unit =
    successfulTestInputs.fromRoot { root =>
      val output = os.proc(TestUtil.cli, "test", TestUtil.extraOptions, ".", "--js").call(cwd = root).out.text
      expect(output.contains("Hello from tests"))
    }

  if (TestUtil.canRunJs)
    test("successful test JS") {
      successfulJsTest()
    }

  def successfulNativeTest(): Unit =
    successfulTestInputs.fromRoot { root =>
      val output = os.proc(TestUtil.cli, "test", TestUtil.extraOptions, ".", "--native").call(cwd = root).out.text
      expect(output.contains("Hello from tests"))
    }

  if (TestUtil.canRunNative)
    test("successful test native") {
      successfulNativeTest()
    }

  test("failing test") {
    failingTestInputs.fromRoot { root =>
      val output = os.proc(TestUtil.cli, "test", TestUtil.extraOptions, ".").call(cwd = root, check = false).out.text
      expect(output.contains("Hello from tests"))
    }
  }

  def failingJsTest(): Unit =
    failingTestInputs.fromRoot { root =>
      val output = os.proc(TestUtil.cli, "test", TestUtil.extraOptions, ".", "--js").call(cwd = root, check = false).out.text
      expect(output.contains("Hello from tests"))
    }

  if (TestUtil.canRunJs)
    test("failing test JS") {
      failingJsTest()
    }

  def failingNativeTest(): Unit =
    failingTestInputs.fromRoot { root =>
      val output = os.proc(TestUtil.cli, "test", TestUtil.extraOptions, ".", "--native").call(cwd = root, check = false).out.text
      expect(output.contains("Hello from tests"))
    }

  if (TestUtil.canRunNative)
    test("failing test native") {
      failingNativeTest()
    }

  test("failing test return code") {
    failingTestInputs.fromRoot { root =>
      val res = os.proc(TestUtil.cli, "test", TestUtil.extraOptions, ".").call(
        cwd = root,
        stdin = os.Inherit,
        stdout = os.Inherit,
        stderr = os.Inherit,
        check = false
      )
      expect(res.exitCode == 1)
    }
  }

  test("utest") {
    successfulUtestInputs.fromRoot { root =>
      val output = os.proc(TestUtil.cli, "test", TestUtil.extraOptions, ".").call(cwd = root).out.text
      expect(output.contains("Hello from tests"))
    }
  }

  def utestJs(): Unit =
    successfulUtestJsInputs.fromRoot { root =>
      val output = os.proc(TestUtil.cli, "test", TestUtil.extraOptions, ".", "--js").call(cwd = root).out.text
      expect(output.contains("Hello from tests"))
    }

  if (TestUtil.canRunJs)
    test("utest JS") {
      utestJs()
    }

  def utestNative(): Unit =
    successfulUtestNativeInputs.fromRoot { root =>
      val output = os.proc(TestUtil.cli, "test", TestUtil.extraOptions, ".", "--native").call(cwd = root).out.text
      expect(output.contains("Hello from tests"))
    }

  if (TestUtil.canRunNative)
    test("utest native") {
      utestNative()
    }

  test("junit") {
    successfulJunitInputs.fromRoot { root =>
      val output = os.proc(TestUtil.cli, "test", TestUtil.extraOptions, ".").call(cwd = root).out.text
      expect(output.contains("Hello from tests"))
    }
  }

  test("several tests") {
    severalTestsInputs.fromRoot { root =>
      val output = os.proc(TestUtil.cli, "test", TestUtil.extraOptions, ".").call(cwd = root).out.text
      expect(output.contains("Hello from tests1"))
      expect(output.contains("Hello from tests2"))
    }
  }

  val platforms = {
    val maybeJs = if (TestUtil.canRunJs) Seq("JS" -> Seq("--js")) else Nil
    val maybeNative = if (TestUtil.canRunNative) Seq("Native" -> Seq("--native")) else Nil
    Seq("JVM" -> Nil) ++ maybeJs ++ maybeNative
  }

  for ((platformName, platformArgs) <- platforms)
    test(s"test framework arguments $platformName") {
      val inputs = TestInputs(
        Seq(
          os.rel / "MyTests.scala" ->
            """import $ivy.`org.scalatest::scalatest::3.2.9`
              |import org.scalatest._
              |import org.scalatest.flatspec._
              |import org.scalatest.matchers._
              |
              |class Tests extends AnyFlatSpec with should.Matchers {
              |  "A thing" should "thing" in {
              |    assert(2 + 2 == 4)
              |  }
              |}
              |""".stripMargin
        )
      )
      inputs.fromRoot { root =>
        val baseRes = os.proc(TestUtil.cli, "test", TestUtil.extraOptions, platformArgs, ".").call(cwd = root)
        val baseOutput = baseRes.out.text
        expect(baseOutput.contains("A thing"))
        expect(baseOutput.contains("should thing"))
        val baseShouldThingLine = baseRes.out.lines().find(_.contains("should thing")).getOrElse(???)
        expect(!baseShouldThingLine.contains("millisecond"))

        val res = os.proc(TestUtil.cli, "test", TestUtil.extraOptions, platformArgs, ".", "--", "-oD").call(cwd = root)
        val output = res.out.text
        expect(output.contains("A thing"))
        expect(output.contains("should thing"))
        val shouldThingLine = res.out.lines().find(_.contains("should thing")).getOrElse(???)
        expect(shouldThingLine.contains("millisecond"))
      }
    }

  for ((platformName, platformArgs) <- platforms)
    test(s"custom test framework $platformName") {
      val inputs = TestInputs(
        Seq(
          os.rel / "MyTests.scala" ->
            """import $ivy.`com.lihaoyi::utest::0.7.9`, utest._
              |
              |object MyTests extends TestSuite {
              |  val tests = Tests {
              |    test("foo") {
              |      assert(2 + 2 == 4)
              |      println("Hello from " + "tests")
              |    }
              |  }
              |}
              |""".stripMargin,
          os.rel / "CustomFramework.scala" ->
            """package custom
              |
              |class CustomFramework extends utest.runner.Framework {
              |  override def setup(): Unit =
              |    println("Hello from CustomFramework")
              |}
              |""".stripMargin
        )
      )
      inputs.fromRoot { root =>
        val baseRes = os.proc(TestUtil.cli, "test", TestUtil.extraOptions, platformArgs, ".").call(cwd = root)
        val baseOutput = baseRes.out.text
        expect(baseOutput.contains("Hello from tests"))
        expect(!baseOutput.contains("Hello from CustomFramework"))

        val res = os.proc(TestUtil.cli, "test", TestUtil.extraOptions, platformArgs, ".", "--test-framework", "custom.CustomFramework").call(cwd = root)
        val output = res.out.text
        expect(output.contains("Hello from tests"))
        expect(output.contains("Hello from CustomFramework"))
      }
    }

}