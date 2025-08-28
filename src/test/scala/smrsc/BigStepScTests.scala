package smrsc

import org.scalatest.funsuite.AnyFunSuite
import smrsc.Graph._

class BigStepScTests extends AnyFunSuite:

  object TestSc extends BigStepSc[Int] with TestScWorld {}

  import TestSc._

  val gs3 = List(
    Forth(0, List(Forth(1, List(Forth(2, List(Back(0), Back(1))))))),
    Forth(0, List(Forth(1, List(Forth(2, List(Back(1))))))),
    Forth(0, List(Forth(1, List(Forth(2, List(Forth(3, List(Back(0), Back(2))))))))),
    Forth(0, List(Forth(1, List(Forth(2, List(Forth(3, List(Back(2))))))))))

  val l1: LazyGraph[C] = lazy_mrsc(0)

  test(testName = "naive mrsc") {
    assert(naive_mrsc(0) == gs3)
  }

  test(testName = "lazy mrsc") {
    assert(unroll(l1) == gs3)
  }

  test(testName = "min size cl") {
    assert(unroll(cl_min_size(l1)) ==
      List(Forth(0, List(Forth(1, List(Forth(2, List(Back(1)))))))))
  }

