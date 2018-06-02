package smrsc.counters

import org.scalatest.FunSuite
import smrsc.Graph._

class ProtocolTests extends FunSuite {

  def runMinSc(cnt: CountersWorld, maxN: Int, maxDepth: Int): Unit = {
    val sc = CountersSc(cnt, maxN, maxDepth)
    val l = sc.lazy_mrsc(cnt.start)
    val sl = cl_empty_and_bad(cnt.isUnsafe)(l)
    val ml = cl_min_size(sl)
    val mg = unroll(ml).head
    println(mg)
  }

  test(testName = "Synapse") {
    runMinSc(Synapse, maxN = 3, maxDepth = 10)
  }

}
