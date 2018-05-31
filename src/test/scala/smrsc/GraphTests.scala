package smrsc

import org.scalatest.FunSuite
import smrsc.Graph._

class GraphTests extends FunSuite {

  type IGraph = Graph[Int]
  type ILazyGraph = LazyGraph[Int]

  def ibad(c: Int): Boolean =
    c < 0

  val g1: IGraph =
    Forth(1, List(
      Back(1),
      Forth(2, List(
        Back(1),
        Back(2)))))

  val g_bad_forth: IGraph =
    Forth(1, List(
      Back(1),
      Forth(-2, List(
        Back(3),
        Back(4)))))

  val g_bad_back: IGraph =
    Forth(1, List(
      Back(1),
      Forth(2, List(
        Back(3),
        Back(-4)))))

  val l2: ILazyGraph =
    Build(1, List(
      List(Build(2,
        List(List(Stop(1), Stop(2))))),
      List(Build(3,
        List(List(Stop(3), Stop(1)))))))

  val gs2: List[IGraph] = List(
    Forth(1, List(Forth(2, List(Back(1), Back(2))))),
    Forth(1, List(Forth(3, List(Back(3), Back(1))))))

  val l_empty: ILazyGraph =
    Build(1, List(
      List(Stop(2)),
      List(Build(3,
        List(
          List(Stop(4), Empty))))))

  test(testName = "unroll") {
    assert(unroll(l2) == gs2)
  }

  test(testName = "!bad") {
    assert(!bad_graph(ibad)(g1))
  }

  test(testName = "bad forth") {
    assert(bad_graph(ibad)(g_bad_forth))
  }

  test(testName = "bad back") {
    assert(bad_graph(ibad)(g_bad_back))
  }

  test(testName = "cl_empty") {
    assert(cl_empty(l_empty) == Build(1, List(List(Stop(2)))))
  }

  val l_bad_forth: ILazyGraph =
    Build(1, List(List(
      Stop(1),
      Build(-2, List(List(
        Stop(3),
        Stop(4)))))))

  test(testName = "lazy bad forth") {
    assert(cl_bad_conf(ibad)(l_bad_forth) ==
      Build(1, List(List(Stop(1), Empty))))
  }

  test(testName = "lazy bad forth cl") {
    assert(cl_empty_and_bad(ibad)(l_bad_forth) ==
      Empty)
  }

  val l_bad_back: ILazyGraph =
    Build(1, List(List(
      Stop(1),
      Build(2, List(List(
        Stop(3),
        Stop(-4)))))))

  test(testName = "lazy bad back") {
    assert(cl_bad_conf(ibad)(l_bad_back) ==
      Build(1, List(List(Stop(1), Build(2, List(List(Stop(3), Empty)))))))
  }

  test(testName = "lazy bad back cl") {
    assert(cl_empty_and_bad(ibad)(l_bad_back) ==
      Empty)
  }

  test(testName = "graph size") {
    assert(graph_size(g1) == 5)
  }

  val l3: ILazyGraph =
    Build(1, List(
      List(Build(2,
        List(List(Stop(1), Stop(2))))),
      List(Build(3,
        List(List(Stop(4)))))))

  test(testName = "min size cl") {
    assert(cl_min_size(l3) == (5,
      Build(1, List(
        List(Build(3,
          List(List(Stop(4)))))))))
  }

  test(testName = "min size cl unroll") {
    val (k, min_l) = cl_min_size(l3)
    val min_g = unroll(min_l).head
    assert(min_g ==
      Forth(1,
        List(Forth(3,
          List(Back(4))))))
  }


}
