package smrsc

import org.scalatest.FunSuite

class BigStepSc8LTests extends FunSuite {

  object TestSc8 extends BigStepSc[Int]
    with BigStepSс8L[Int] with TestScWorld {}

  import TestSc8._

  test(testName = "lazy_mrsc ~ build_cograph andThen prune") {
    val l: LazyGraph[C] = lazy_mrsc(0)
    val plc = prune(build_cograph(0))
    assert(l == plc)
  }
}
