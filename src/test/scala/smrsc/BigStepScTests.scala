package smrsc

import org.scalatest.FunSuite

import smrsc.Graph._

object TestSc extends BigStepSc {

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

class BigStepScTests extends FunSuite {

  import smrsc.TestSc._

  val gs3 = List(
    Forth(0,List(Forth(1,List(Forth(2,List(Back(0), Back(1))))))),
    Forth(0,List(Forth(1,List(Forth(2,List(Back(1))))))),
    Forth(0,List(Forth(1,List(Forth(2,List(Forth(3,List(Back(0), Back(2))))))))),
    Forth(0,List(Forth(1,List(Forth(2,List(Forth(3,List(Back(2))))))))))

  val l1: LazyGraph[C] = lazy_mrsc(0)

  test(testName = "naive mrsc") {
    assert(naive_mrsc(0) == gs3)
  }

  test(testName = "lazy mrsc") {
    assert(unroll(l1) == gs3)
  }

  test(testName = "min size cl") {
    assert(unroll(cl_min_size(l1)) ==
      List(Forth(0,List(Forth(1,List(Forth(2,List(Back(1)))))))))
  }

}
