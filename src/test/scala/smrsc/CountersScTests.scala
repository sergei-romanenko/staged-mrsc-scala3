package smrsc

import org.scalatest.FunSuite

import smrsc.Graph._
import smrsc.counters._

object CountersTestObj extends CountersSc with CountersWorld {

  val maxN: Int = 3
  override val maxDepth: Int = 10

  val start: C = List(2, 0)
  val rules: List[Rule] = List(
    { case List(i, j) if i >= 1 => List(i - 1, j + 1) },
    { case List(i, j) if j >= 1 => List(i + 1, j - 1) }
  )

  val isUnsafe: C => Boolean = _ => false
}

class CountersScTests extends FunSuite {

  import CountersTestObj._

  val mg: Graph[C] =
    Forth(List(2, 0), List(
      Forth(List(W, W), List(
        Back(List(W, W)),
        Back(List(W, W))))))

  test(testName = "naive mrsc ~ lazy mrsc") {
    val gs = naive_mrsc(start)
    //println(s"gs.length ==${gs.length}")
    val l = lazy_mrsc(start)
    assert(unroll(l) == gs)
    val ml = cl_min_size(l)
    assert(unroll(ml._2).head == mg)
  }

}
