# Counting without generation

The main idea of staged supercompilation consists in
replacing the analysis of residual graphs with the analysis
of the program that generates the graphs.

Gathering statistics about graphs is just a special case of
such analysis. For example, it is possible to count the number of
residual graphs that would be produced without actually generating
the graphs.

## Counting residual graphs

Technically, we can define a function `length_unroll` that analyses
lazy graphs such that

```text
    length_unroll(l) == unroll(l).length
```

Here is its definition (see `Statistics.scala`).

```scala
  def length_unroll[C]: LazyGraph[C] => Long = {
    case Empty => 0
    case Stop(c) => 1
    case Build(c, lss) =>
      lss.map(_.map(length_unroll).foldLeft(1L)(_ * _)).sum
  }
```

## Counting nodes in collections of residual graphs

We can define a function `size_unroll`, such that

```text
    size_unroll(l) == (unroll(l).length , unroll(l).map(graph_size).sum)
```

`size_unroll` computes the number of graphs represented by `l` and
the total number of nodes in the graphs represented by `l`.
It does so to avoid calling `length_unroll` (which would lead to
the same values being calculated several times).

Here is the definition of `size_unroll` (see `Statistics.agda`).

```scala
  def size_unroll[C]: LazyGraph[C] => (Long, Long) = {
    case Empty => (0, 0)
    case Stop(c) => (1, 1)
    case Build(c, lss) =>
      lss.foldLeft((0L, 0L)) {
        case ((k, n), ls) =>
          val (k1, n1) = size_unroll_ls(ls)
          (k + k1, n + k1 + n1)
      }
  }

  def size_unroll_ls[C]: List[LazyGraph[C]] => (Long, Long) =
    _.foldLeft((1L, 0L)) {
      case ((k, n), l) =>
        val (k1, n1) = size_unroll(l)
        (k * k1, k * n1 + k1 * n)
    }

```
