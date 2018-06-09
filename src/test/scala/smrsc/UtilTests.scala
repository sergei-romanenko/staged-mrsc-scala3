package smrsc

import org.scalatest.FunSuite

class UtilTests extends FunSuite {

  def testCartesian(g: List[List[Int]], e: List[List[Int]]): Unit = {
    val actual = cartesian(g)
    assert(actual == e)
  }

  test(testName = "cartesian0") {
    testCartesian(
      List(), List(List()))
  }

  test(testName = "cartesian1") {
    testCartesian(
      List(List(), List(10, 20)), List())
  }

  test(testName = "cartesian2") {
    testCartesian(
      List(List(1, 2), List()), List())
  }

  test(testName = "cartesian3") {
    testCartesian(
      List(List(1, 2)), List(List(1), List(2)))
  }

  test(testName = "cartesian4") {
    testCartesian(
      List(List(1), List(10, 20)), List(List(1, 10), List(1, 20)))
  }

  test(testName = "cartesian5") {
    testCartesian(
      List(List(1, 2), List(10, 20, 30), List(100, 200)),
      List(
        List(1, 10, 100), List(1, 10, 200),
        List(1, 20, 100), List(1, 20, 200),
        List(1, 30, 100), List(1, 30, 200),
        List(2, 10, 100), List(2, 10, 200),
        List(2, 20, 100), List(2, 20, 200),
        List(2, 30, 100), List(2, 30, 200)))
  }
}
