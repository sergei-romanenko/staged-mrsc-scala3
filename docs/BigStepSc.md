# A model of big-step multi-result supercompilation

We have formulated an idealized model of big-step multi-result supercompilation.
This model is rather abstract, and yet it can be instantiated to produce
runnable supercompilers.

Given an initial configuration `c`, a supercompiler produces a list of
"residual" graphs of configurations:
```
    g[1], ... , g[k]
```

What is a "graph of configurations"?

## Graphs of configurations

Graphs of configurations are supposed to represent "residual programs" and are
defined in Scala (see `Graph.scala`) in the following way:

```scala
sealed trait Graph[C]

case class Back[C](c: C) extends Graph[C]

case class Forth[C](c: C, gs: List[Graph[C]]) extends Graph[C]
```

Technically, a `Graph[C]` is a tree, with `Back` nodes being
references to parent nodes.

A graph's nodes contain configurations. Here we abstract away
from the concrete structure of configurations.
In this model the arrows in the graph carry no information,
because, this information can be kept in nodes.
(Hence, this information is supposed to be encoded inside
"configurations".)

To simplify the machinery, back-nodes in this model of
supercompilation do not contain explicit references
to parent nodes. Hence, `Back(c)` means that `c` is foldable
to a parent configuration (perhaps, to several ones).

* Back-nodes are produced by folding a configuration to another
  configuration in the history.

* Forth-nodes are produced either

    + by decomposing a configuration into a number of other configurations
      (e.g. by driving or taking apart a let-expression), or

    + by rewriting a configuration by another one (e.g. by generalization,
      introducing a let-expression or applying a lemma during
      two-level supercompilation).

## "Worlds" of supercompilation

The knowledge about the input language a supercompiler deals with
is represented by a "world of supercompilation", which extends the
following trait
```scala
trait ScWorld[C] {

  type History = List[C]

  def isDangerous(h: History): Boolean

  def isFoldableTo(c1: C, c2: C): Boolean

  def develop(c: C): List[List[C]]

  def isFoldableToHistory(c: C, h: History): Boolean =
    h.exists(isFoldableTo(c, _))
}
```
in which

* `C` is the type of "configurations". Note that configurations are
   not required to be just expressions with free variables! In general,
   they may represent sets of states in any form/language and as well may
   contain any _additional_ information.

* `isDangerous` is a "whistle", which is used to ensure termination of
  supercompilation. `isDangerous(h)` means that the history `h`
  is too large or too sophisticated, so that extending `h` may lead to
  a non-terminating supercompilation.

* `isFoldableTo(c1, c2)` means that the configuration`c1` is foldable to
  the configuration `c2`. (In such cases `c2` is usually said to be "more
  general" than `c1`.)

* `develop` is a function that gives a number of possible decompositions of
  a configuration. Let `c` be a configuration and `cs` a list of configurations
  such that `cs ∈ develop(c)`. Then `c` can be "reduced to" (or "decomposed
  into") configurations `cs`.

Suppose that driving is deterministic and, given a configuration `c`,
produces a list of configurations `drive(c)`. Suppose that rebuilding
(generalization, application of lemmas) is non-deterministic and
`rebuild(c)` is the list of configurations that can be produced by
rebuilding. Then (in this special case) `develop` can be implemented as
follows:

```scala
  override def develop(c: C): List[List[C]] =
    List(drive(c)) ::: rebuild(c).map(List(_))
```

Note that, in addition to abstract members, `ScWorld[C]` contains a few
concrete members.

* `History` is a list of configuration that have been produced
  in order to reach the current configuration.

* `isFoldableToHistory(c, h)` means that `c` is foldable to a configuration in
  the history `h`.


## Graphs with labeled edges

If we need labeled edges in the graph of configurations, the labels can be
hidden inside configurations. (Recall that "configurations" do not have to be
just symbolic expressions, as they can contain any additional information.)

For example, in case of supercompilation for a functional language,
configurations can be declared as

```scala
type C = (Expr, Option[Contraction])
```
where `Expr` is an "expression" with free variables and `Contraction` is
a test that has to be performed in order to reach this configuration.

## A function for computing Cartesian products

The functional specification of big-step multi-result supercompilation
considered in the following section is based on the function
`cartesian`, defined in the file `package.scala`:

```scala
  def cartesian[A]: List[List[A]] => List[List[A]] =
    _.foldRight[List[List[A]]](List(Nil)) {
      case (xs, yss) =>
        xs.flatMap(x => yss.map(x :: _))
    }
```

`cartesian` takes as input a list of lists `xss`. Each list `xs ∈ xss`
represents the set of possible values of the correspondent component.

Namely, suppose that `xss` has the form

```text
    xs[1] :: xs[2] :: ... :: xs[k] :: Nil
```

Then `cartesian` returns a list containing all possible lists of
the form

```text
    x[1] :: x[2] :: ... :: x[k] :: Nil
```

where `x[i] ∈ xs[i]`. In Scala, this property of `cartesian` is
formulated as follows:

## A functional specification of big-step multi-result supercompilation

A functional specification of big-step multi-result supercompilation
is given in the form of a function (in `BigStepSc.scala`)
that takes the initial configuration `c` and returns a list of residual
graphs:

```scala
  def naive_mrsc(c: C): List[Graph[C]] =
    naive_mrsc_loop(Nil)(c)
```

`naive_mrsc` is defined in terms of a more general function
`naive_mrsc_loop`, which takes an additional arguments: a history `h`
(and `naive_mrsc` calls `naive_mrsc_loop` with the empty history).

```scala
  def naive_mrsc_loop(h: History)(c: C): List[Graph[C]] =
    if (isFoldableToHistory(c, h))
      List(Back(c))
    else if (isDangerous(h))
      List()
    else
      develop(c)
        .flatMap(cs => cartesian(cs.map(naive_mrsc_loop(c :: h))))
        .map(Forth(c, _))
```

The definition of `naive_mrsc` is straightforward.

* If `c` is foldable to the history `h`, a back-node is generated
  and the function terminates.

* Otherwise, if `isDangerous(h)` (i.e. the history `h` is dangerous),
  the function terminates producing no graphs.

* Otherwise, `h` is not dangerous, and the configuration `c`
  can be decomposed.

* Thus `develop(c)` returns a list of lists of configurations. The function
  considers each `cs ∈ develop(c)`, and, for each `c1 ∈ cs` recursively
  calls itself in the following way:  
      `naive_mrsc_loop (c :: h) c1`  
  producing a list of residual graphs `gs'`. So, `cs` is
  transformed into gss, a list of lists of graphs. Note that
  `cs.length cs == gss.length`.

* Then the function computes cartesian product `cartesian(gss)`,
  to produce a list of lists of graphs. Then the results
  corresponding to each `cs ∈ develop(c)` are concatenated (by `flatMap`).

* At this moment the function has obtained a list of lists of graphs,
  and calls `map(Forth(c, _)` to turn each graph list into a forth-node.

## Does `naive_mrsc_loop` always terminates?

The problem with `naive_mrsc_loop` is that in the recursive call

```scala
    naive_mrsc_loop (c :: h) c1
```

the history grows (`h` becomes `c :: h`), and the configuration
is replaced with another configuration of unknown size (`c` becomes
`c1`). Hence, these parameters do not become "structurally smaller".

Hence, `isDangerous` is expected to eventually stop the process of
supercompilation.

The most trivial solution is to define `isDangerous` as

```scala
  override def isDangerous(h: History): Boolean =
    h.length >= maxDepth
``` 
