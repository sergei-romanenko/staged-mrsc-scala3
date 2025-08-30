package smrsc.counters

import org.scalatest.funsuite.AnyFunSuite
import smrsc.Graph._
import smrsc.Statistics._
import smrsc.{BigStepSc, GraphPrettyPrinter}

class ProtocolTests extends AnyFunSuite:

  def runMinSc(cw: CountersWorld, m: Int, d: Int): Unit =
    val name = cw.getClass.getName.split("[\\.\\$]").last
    print(s"\n$name ")
    val sc = new BigStepSc[List[NW]] with CountersScWorld:
      val cnt: CountersWorld = cw
      val maxN: Int = m
      val maxDepth: Int = d
    val l = sc.lazy_mrsc(sc.cnt.start)
    val sl = cl_empty_and_bad(cw.isUnsafe)(l)
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

  ignore(testName = "MOESI") {
    runMinSc(MOESI, m = 3, d = 10)
  }

  ignore(testName = "Illinois") {
    runMinSc(Illinois, m = 3, d = 10)
  }

  test(testName = "Berkley") {
    runMinSc(Berkley, m = 3, d = 10)
  }

  ignore(testName = "Firefly") {
    runMinSc(Firefly, m = 3, d = 10)
  }

  ignore(testName = "Futurebus") {
    runMinSc(Futurebus, m = 3, d = 8)
  }

  ignore(testName = "Xerox") {
    runMinSc(Xerox, m = 3, d = 10)
  }

  test(testName = "DataRace") {
    runMinSc(DataRace, m = 3, d = 10)
  }
