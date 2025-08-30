package smrsc

object Statistics:

  //
  // Counting without generation
  //
  //
  // The main idea of staged supercompilation consists in
  // replacing the analysis of residual graphs with the analysis
  // of the program that generates the graphs.
  //
  // Gathering statistics about graphs is just a special case of
  // such analysis. For example, it is possible to count the number of
  // residual graphs that would be produced without actually generating
  // the graphs.
  //
  // Technically, we can define a function `length_unroll(c)` that analyses
  // lazy graphs such that
  //   length_unroll(l) == unroll(l).length

  def length_unroll[C]: LazyGraph[C] => Long =
    case Empty   => 0
    case Stop(c) => 1
    case Build(c, lss) =>
      lss.map(_.map(length_unroll).foldLeft(1L)(_ * _)).sum

  //
  // Counting nodes in collections of graphs
  //
  // Let us find a function `size_unroll(l)`, such that
  //   size_unroll(l) == (unroll(l).length , unroll(l).map(graph_size).sum)
  //

  def size_unroll[C]: LazyGraph[C] => (Long, Long) =
    case Empty   => (0, 0)
    case Stop(c) => (1, 1)
    case Build(c, lss) =>
      lss.foldLeft((0L, 0L)) { case ((k, n), ls) =>
        val (k1, n1) = size_unroll_ls(ls)
        (k + k1, n + k1 + n1)
      }

  def size_unroll_ls[C]: List[LazyGraph[C]] => (Long, Long) =
    _.foldLeft((1L, 0L)) { case ((k, n), l) =>
      val (k1, n1) = size_unroll(l)
      (k * k1, k * n1 + k1 * n)
    }
