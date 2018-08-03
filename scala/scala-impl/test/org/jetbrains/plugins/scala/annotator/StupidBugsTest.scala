package org.jetbrains.plugins.scala.annotator

class StupidBugsTest extends ScalaHighlightingTestBase {
  def testSingletonType(): Unit = {
    val code =
      """
        |object Test {
        |  def f[T](xs: T*) = ()
        |
        |  val x = "abc"
        |
        |  f[x.type](x)
        |}
      """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }
}
