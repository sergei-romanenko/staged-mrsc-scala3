package smrsc

import org.scalatest.FunSuite
import smrsc.Graph._
import smrsc.Statistics._

class StatisticsTests extends FunSuite {

  object TestSc extends BigStepSc[Int] with TestScWorld {}

  import TestSc._

  val l1: LazyGraph[C] = lazy_mrsc(0)
  val ul1: List[Graph[C]] = unroll(l1)

  test(testName = "len_unroll") {
    assert(length_unroll(l1) == ul1.length)
  }

  test(testName = "size_unroll") {
    assert(size_unroll(l1) == (ul1.length , ul1.map(graph_size).sum))
  }
}
