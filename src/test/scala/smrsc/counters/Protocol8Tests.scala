package smrsc.counters

import org.scalatest.FunSuite
import smrsc.Graph._
import smrsc.GraphPrettyPrinter

class Protocol8Tests extends FunSuite {

  def runMinSc(cnt: CountersWorld, maxN: Int, maxDepth: Int): Unit = {
    val name = cnt.getClass.getName.split("[\\.\\$]").last
    println(s"\n$name")
    val sc = CountersSc8(cnt, maxN, maxDepth)
    val l8 = sc.build_cograph(cnt.start)
    val sl8 = sc.cl8_bad_conf(cnt.isUnsafe)(l8)
    val sl = sc.prune(sl8)
    val ml = cl_min_size(sl)
    val mg = unroll(ml).head
    println(GraphPrettyPrinter.toString(mg))
  }

  test(testName = "Synapse") {
    runMinSc(Synapse, maxN = 3, maxDepth = 10)
  }

  test(testName = "MSI") {
    runMinSc(MSI, maxN = 3, maxDepth = 10)
  }

  test(testName = "MOSI") {
    runMinSc(MOSI, maxN = 3, maxDepth = 10)
  }

  ignore(testName = "ReaderWriter") {
    runMinSc(ReaderWriter, maxN = 3, maxDepth = 10)
  }

  test(testName = "MESI") {
    runMinSc(MESI, maxN = 3, maxDepth = 10)
  }

  test(testName = "MOESI") {
    runMinSc(MOESI, maxN = 3, maxDepth = 10)
  }

  test(testName = "Illinois") {
    runMinSc(Illinois, maxN = 3, maxDepth = 10)
  }

  test(testName = "Berkley") {
    runMinSc(Berkley, maxN = 3, maxDepth = 10)
  }

  test(testName = "Firefly") {
    runMinSc(Firefly, maxN = 3, maxDepth = 10)
  }

  ignore(testName = "Futurebus") {
    runMinSc(Futurebus, maxN = 3, maxDepth = 10)
  }

  test(testName = "Xerox") {
    runMinSc(Xerox, maxN = 3, maxDepth = 10)
  }

  test(testName = "DataRace") {
    runMinSc(DataRace, maxN = 3, maxDepth = 10)
  }

}
