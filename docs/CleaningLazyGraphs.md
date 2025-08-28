# Cleaning lazy graphs

A function `clean` is said to be a "cleaner" if for any lazy
graphs `l`

```text
     unroll(clean(l)) ⊆ unroll(l)
```

Suppose that a function `filter` filters bags of graphs,
removing "bad" graphs, so that

```text
     filter(unroll(l))
```

generates the bag of "good" graphs. Let `clean` be a cleaner such that

```text
     filter ∘ unroll ≗ unroll ∘ clean
```

Then we can replace filtering of graphs with cleaning of
lazy graphs

```text
     filter ∘ naive_mrsc ≗ unroll ∘ clean ∘ lazy_mrsc
```

In `Graph.agda`, there are defined a number of filters and
corresponding cleaners.

## Filter `fl_bad_conf` and cleaner `cl_bad_conf`

```scala
  def fl_bad_conf[C](bad: C => Boolean)(gs: List[Graph[C]]): List[Graph[C]]

  def cl_bad_conf[C](bad: C => Boolean): LazyGraph[C] => LazyGraph[C]
```

Configurations represent states of a computation process.
Some of these states may be "bad" with respect to the problem
that is to be solved by means of supercompilation.

Given a predicate `bad` that returns `True` for "bad" configurations,
`fl_bad_conf(bad)(gs)` removes from `gs` the graphs that contain
at least one "bad" configuration.

The cleaner `cl_bad_conf` corresponds to the filter `fl_bad_conf`.
`cl_bad_conf` exploits the fact that "badness" is monotonic,
in the sense that a single "bad" configuration spoils the whole
graph.

`cl_bad_conf` is correct with respect to `fl_bad_conf`:

```text
  unroll ∘ cl_bad_conf bad ≗ fl_bad_conf bad ∘ unroll
```

There exists a proof of this theorem formalized in Agda.

It is instructive to take a look at the implementation of
`cl_bad_conf` in `Graph.agda`, to get the general idea of
how cleaners are really implemented:

```scala
  def cl_bad_conf[C](bad: C => Boolean): LazyGraph[C] => LazyGraph[C] = {
    case Empty => Empty
    case Stop(c) =>
      if (bad(c)) Empty else Stop(c)
    case Build(c, lss) =>
      if (bad(c)) Empty else Build(c, lss.map(_.map(cl_bad_conf(bad))))
  }
```

## Cleaner `cl_empty`

`cl_empty` is a cleaner that removes subtrees of a lazy graph that
represent empty sets of graphs.

```scala
  def cl_empty[C]: LazyGraph[C] => LazyGraph[C]
```

`cl_bad_conf` is correct with respect to `unroll`:

```text
  unroll(cl_empty(l)) == unroll(l)
```

There exists a proof of this theorem formalized in Agda.

## Cleaner `cl_min_size`

The size of a graph is calculated by the function `graph-size`:

```scala
  def graph_size[C]: Graph[C] => Long = {
    case Back(_) => 1L
    case Forth(_, gs) =>
      1L + gs.map(graph_size).sum
  }
```

The function `sel_min_size`

```scala
  def sel_min_size[C]: LazyGraph[C] => (Long, LazyGraph[C])
```

takes as input a lazy graph`l` and returns either `(0L , Empty)`,
if `unroll(l)` contains no graphs, or a pair `(k , l1)`,
where `l1` is a lazy graph, representing a single graph `g1`
of minimal size `k`.

More formally,

* `unroll(l1) == List(g1)`.
* `graph_size(g1) == k`
* `k ≤ graph_size(g)` for all `g ∈ unroll(l)`.

The main idea behind `sel_min_size` is that, if we have a node
`build(c, lss)`, then we can clean each `ls ∈ lss`.

Let us consider an `ls ∈ lss`. We can clean with `sel_min_size` each
`l ∈ ls` to obtain `ls1`, a new list of lazy graphs .
If `Empty ∈ ls1`, we replace the node `build(c, lss)` with `Empty`.
The reason is that computing the Cartesian product
for `ls1` would produce an empty set of results. Otherwise,
we replace `build(c, lss)` with `build(c, lss1)`.

A good thing about `sel_min_size` is that it cleans any lazy graph `l`
in linear time with respect to the size of `l`.

Having defined `sel_min_size`, we can define the function `cl_min_size`

```scala
  def cl_min_size[C] (l : LazyGraph[C]): LazyGraph[C] =
    sel_min_size(l)._2
```

that takes as input a lazy graph`l` and returns either `Empty`,
if `unroll(l)` is `Nil`, or a lazy graph `l1`, representing a single
graph `g1` of minimal size.
