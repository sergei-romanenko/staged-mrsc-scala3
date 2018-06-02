package smrsc.counters

import org.scalatest.FunSuite
import smrsc.Graph._
import smrsc.GraphPrettyPrinter

class ProtocolTests extends FunSuite {

  def runMinSc(cnt: CountersWorld, maxN: Int, maxDepth: Int): Unit = {
    val name = cnt.getClass.getName.split("[\\.\\$]").last
    println(s"\n$name")
    val sc = CountersSc(cnt, maxN, maxDepth)
    val l = sc.lazy_mrsc(cnt.start)
    val sl = cl_empty_and_bad(cnt.isUnsafe)(l)
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

//  test(testName = "ReaderWriter") {
//    runMinSc(ReaderWriter, maxN = 3, maxDepth = 10)
//  }

  test(testName = "MESI") {
    runMinSc(MESI, maxN = 3, maxDepth = 10)
  }

//  test(testName = "MOESI") {
//    runMinSc(MOESI, maxN = 3, maxDepth = 10)
//  }

//  test(testName = "Illinois") {
//    runMinSc(Illinois, maxN = 3, maxDepth = 10)
//  }

  test(testName = "Berkley") {
    runMinSc(Berkley, maxN = 3, maxDepth = 10)
  }

//  test(testName = "Firefly") {
//    runMinSc(Firefly, maxN = 3, maxDepth = 10)
//  }

//  test(testName = "Futurebus") {
//    runMinSc(Futurebus, maxN = 3, maxDepth = 10)
//  }

//  test(testName = "Xerox") {
//    runMinSc(Xerox, maxN = 3, maxDepth = 10)
//  }

  test(testName = "DataRace") {
    runMinSc(DataRace, maxN = 3, maxDepth = 10)
  }

}
