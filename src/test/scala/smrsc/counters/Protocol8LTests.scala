package smrsc.counters

import org.scalatest.funsuite.AnyFunSuite
import smrsc.Graph._
import smrsc.Statistics._
import smrsc.GraphPrettyPrinter
import smrsc.BigStepSс8L

class Protocol8LTests extends AnyFunSuite:

  def runMinSc(cw: CountersWorld, m: Int, d: Int): Unit =
    val name = cw.getClass.getName.split("[\\.\\$]").last
    print(s"\n$name ")
    val sc = new BigStepSс8L[List[NW]] with CountersScWorld:
      val cnt: CountersWorld = cw
      val maxN: Int = m
      val maxDepth: Int = d
    val l8 = sc.build_cograph(cw.start)
    val sl8 = sc.cl8_bad_conf(cw.isUnsafe)(l8)
    val sl = sc.prune(sl8)
    val (len_usl, size_usl) = size_unroll(sl)
    println(s"${len_usl} ${size_usl}")
    val ml = cl_min_size(sl)
    val mg = unroll(ml).head
    println(GraphPrettyPrinter.toString(mg))

  test(testName = "Synapse") {
    runMinSc(Synapse, m = 3, d = 10)
  }

  test(testName = "MSI") {
    runMinSc(MSI, m = 3, d = 10)
  }

  test(testName = "MOSI") {
    runMinSc(MOSI, m = 3, d = 10)
  }

  test(testName = "ReaderWriter") {
    runMinSc(ReaderWriter, m = 3, d = 5)
  }

  test(testName = "MESI") {
    runMinSc(MESI, m = 3, d = 10)
  }

  test(testName = "MOESI") {
    runMinSc(MOESI, m = 3, d = 10)
  }

  test(testName = "Illinois") {
    runMinSc(Illinois, m = 3, d = 10)
  }

  test(testName = "Berkley") {
    runMinSc(Berkley, m = 3, d = 10)
  }

  test(testName = "Firefly") {
    runMinSc(Firefly, m = 3, d = 10)
  }

  ignore(testName = "Futurebus") {
    runMinSc(Futurebus, m = 3, d = 8)
  }

  test(testName = "Xerox") {
    runMinSc(Xerox, m = 3, d = 10)
  }

  test(testName = "DataRace") {
    runMinSc(DataRace, m = 3, d = 10)
  }

