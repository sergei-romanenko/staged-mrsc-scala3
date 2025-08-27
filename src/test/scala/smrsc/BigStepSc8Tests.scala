package smrsc

import org.scalatest.funsuite.AnyFunSuite

class BigStepSc8Tests extends AnyFunSuite {

  object TestSc8 extends BigStepSc[Int]
    with BigStepSс8[Int] with TestScWorld {}

  import TestSc8._

  test(testName = "lazy_mrsc ~ build_cograph andThen prune") {
    val l: LazyGraph[C] = lazy_mrsc(0)
    val plc = prune(build_cograph(0))
    assert(l == plc)
  }
}
