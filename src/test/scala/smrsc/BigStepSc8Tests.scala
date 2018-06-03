package smrsc

import org.scalatest.FunSuite
import smrsc.Graph._

class BigStepSc8Tests extends FunSuite {

  object TestSc8 extends BigStepSc with BigStepSс8 {

    type C = Int

    override def isDangerous(h: History): Boolean =
      h.length > 3

    override def isFoldableTo(c1: C, c2: C): Boolean =
      c1 == c2

    override def develop(c: C): List[List[C]] =
      drive(c) ::: rebuild(c).map(List(_))

    def drive(c: C): List[List[C]] =
      if (c < 2) List() else List(List(0, c - 1), List(c - 1))

    def rebuild(c: C): List[C] =
      List(c + 1)
  }

  import TestSc8._

  test(testName = "lazy_mrsc ~ build_cograph andThen prune") {
    val l: LazyGraph[C] = lazy_mrsc(0)
    val plc = prune(build_cograph(0))
    assert(l == plc)
  }
}
