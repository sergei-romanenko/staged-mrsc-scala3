# Staging big-step multi-result supercompilation

We can decompose the process of supercompilation into two stages

```text
    naive_mrsc ≗ unroll ∘ lazy_mrsc
```

where `unroll` is a unary function, and `f ≗ g` means that `f x = g y`
for all `x`.

At the first stage, `lazy_mrsc` generates a "lazy graph", which, essentially, is
a "program" to be "executed" by `unroll`.

## Lazy graphs of configuration

A `LazyGraph[C]` represents a finite set of graphs
of configurations (whose type is `Graph[C]`).

```scala
sealed trait LazyGraph[+C]

case object Empty extends LazyGraph[Nothing]

case class Stop[C](c: C) extends LazyGraph[C]

case class Build[C](c: C, lss: List[List[LazyGraph[C]]]) extends LazyGraph[C]
```

A lazy graph is a tree whose nodes are "commands" to be executed
by the interpreter `unroll`.

The exact semantics of lazy graphs is given by the function `unroll`
(see `Graph.scala`).

```scala
  def unroll[C](l: LazyGraph[C]): List[Graph[C]] = l match {
    case Empty => Nil
    case Stop(c) => List(Back(c))
    case Build(c, lss) =>
      lss.flatMap(ls => cartesian(ls.map(unroll))).map(Forth(c, _))
  }
```

It can be seen that `Empty` means "generate no graphs", `Stop` means
"generate a back-node and stop".

The most interesting case is a build-node `Build(c, lss)`, where `c` is
a configuration and  `lss` a list of lists of lazy graphs.
Recall that, in general, a configuration can be decomposed
into a list of configurations in several different ways.
Thus, each `ls ∈ lss` corresponds to a decomposition of `c` into
a number of configurations `c[1], ... c[k]`. By supercompiling
each `c[i]` we get a collection of graphs that can be represented
by a lazy graph `ls[i].

So, `unroll` considers each lazy graph in a list of lazy graphs `ls`,
and turns it into a list of graphs.

Thus `unroll` considers all possible decompositions of
a configuration, and for each decomposition computes all possible
combinations of subgraphs by calling `cartesian`.

## A functional specification of lazy multi-result supercompilation

Given a configuration `c`, the function `lazy_mrsc` produces a lazy graph.

`lazy_mrsc` is defined in terms of a more general function `lazy_mrsc_loop`
The general structure of `lazy_mrsc_loop` is [very similar](BigStepSc.md)
to that of `naive_mrsc′`, but, unlike `naive_mrsc`, it does not build
Cartesian products immediately.

```scala
  def lazy_mrsc_loop(h: History)(c: C): LazyGraph[C] =
    if (isFoldableToHistory(c, h))
      Stop(c)
    else if (isDangerous(h))
      Empty
    else
      Build(c,
        develop(c).map(_.map(lazy_mrsc_loop(c :: h))))

  def lazy_mrsc(c: C): LazyGraph[C] =
    lazy_mrsc_loop(Nil)(c)
```

Let us compare the most interesting parts of `naive_mrsc` and `lazy_mrsc`:

```scala
    develop(c)
      .flatMap(cs => cartesian(cs.map(naive_mrsc_loop(c :: h))))
      .map(Forth(c, _))
    ...
    Build(c,
      develop(c).map(_.map(lazy_mrsc_loop(c :: h))))
```

Note that `cartesian` disappears from `lazy_mrsc`.

## Correctness of `lazy_mrsc` and `unroll`

`lazy_mrsc` and `unroll` are correct with respect to `naive_mrsc`.
This can be formulated as follows:

```scala
  naive_mrsc(c) == unroll(lazy_mrsc(c))
```

In other words, for any initial configuraion `c`, `unroll(lazy_mrsc(c))`
returns the same list of graphs (the same graphs in the same order!) as
would return `naive_mrsc(c)`.

There exists a proof of this property formalized in Agda.
