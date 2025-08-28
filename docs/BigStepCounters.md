# An example: big-step supercompilation for counter systems

The abstract model of big-step multi-result supercompilation
presented in `BigStepSc.scala` can be instantiated to produce
runnable supercompilers.

In `Counters.scala` there is implemented a toolkit for
describing counter systems and turning them into working
supercompilers.

## Worlds of counters

A counter system is characterized by two parameters: `k` and `maxN`,
and configurations have simple structure. Any configuration
is a list of fixed length `k`. Each component of this list is
either the symbol `ω` or a natural number `n`, such that `n < maxN`.

In the Scala implementation, `ω` is represented by the object `W` and
an integer `i` by the object `N(i)`. We also define an implicit conversion
that enables write `i` instead of `N(i)`.

`NW` is the type that is the supertype for the values `N(i)` and `W`.

```scala
sealed trait NW {
  def +(comp: NW): NW
  def -(comp: NW): NW
  def >=(i: Int): Boolean
  def ===(i: Int): Boolean
  def isIn(nw: NW): Boolean
}

case class N(i: Int) extends NW { ... }

case object W extends NW { ... }

```

A specification of a counter system is a "world of counters",
which is a trait `CountersWorld`:

```scala
trait CountersWorld {
  import scala.language.implicitConversions
  implicit def int2NOmega(i: Int): NW = N(i)

  type C = List[NW]

  type Rule = PartialFunction[C, C]
  val start: C
  val rules: List[Rule]
  val isUnsafe: C => Boolean
}
```

where

* `C` is the type of configurations.
* `start` is the initial configuration.
* `rules` specifies how to drive a configuration: `c ⇊` returns
  a list of configurations in which `c` is decomposed. (Driving
  in the case of counter systems is deterministic.)
* `isUnsafe` determines which configurations are considered to be
  (semantically) "unsafe".

A rule `r` is a partial function that, if it is applicable to a configuration
`c`, returns a new configuration `r(c)`.

In Scala, `r.lift` is a function that returns `Some(r(c))`, if `r` is
applicable to `c`, or `None` otherwise. Hence, the result of driving
a configuration`c` is produced by the expression `rules.flatMap(_.lift(c))`.
Namely, if `r.lift(c)` returns `None`, `flatMap` just throws this `None` away,
but if `r.lift(c)` returns `Some(c1)`, `flatMap` turns `Some(c1)` into `c1`.
Hence, each applicable rule produces a new configuration.

Then there is defined the trait `CountersScWorld`

```agda
trait CountersScWorld extends ScWorld[List[NW]] {

  val cnt: CountersWorld
  val maxN: Int
  val maxDepth: Int

  ...
}
```

which has a number of abstract members.

* `cnt` is a world of counters.
* `maxN` is the limit on the size of natural numbers in configurations
  (for a component `n` there must hold `n < maxN`).
* `maxDepth` is the limit on the length of histories
  (is used by the whistle).

When instantiated, `CountersScWorld` produces concrete members
for ScWorld[List[NW]].

## Making a supercompiler

In the subfolder `Protocol` there are defined a number of
worlds of supercompilation, which are models of communication
protocols.

In order to be specific, let us consider the test (meaningless) protocol
`TestProtocol` and the corresponding supercompiler defined in
`CountersScTests.scala`.

The protocol is specified by the object `TestProtocol`:

```scala
  object TestProtocol extends CountersWorld {

    val start: C = List(2, 0)
    val rules: List[Rule] = List(
      { case List(i, j) if i >= 1 => List(i - 1, j + 1) },
      { case List(i, j) if j >= 1 => List(i + 1, j - 1) }
    )

    val isUnsafe: C => Boolean = _ => false
  }
```

And then, the corresponding supercompiler is declared as the object
`TestProtocolSc`:

```scala
  object TestProtocolSc extends BigStepSc[List[NW]]
    with CountersScWorld {
    val cnt = TestProtocol
    val maxN = 3
    val maxDepth = 10
  }
```

Now we can write a test that calls `naive_mrsc` to produce a list of
graphs `gs`, and `lazy_mrsc` to produce a lazy graph. Then we check that
`unroll(l) == gs`.

```scala
  test(testName = "naive mrsc ~ lazy mrsc") {
    val gs = naive_mrsc(start)
    //println(s"gs.length ==${gs.length}")
    val l = lazy_mrsc(start)
    assert(unroll(l) == gs)
  }

```

In `Protocol8Tests`, there is the definition of the function `runMinSc`,
which takes as parameters a `CountersWorld`, the values of `maxN` and
`maxDepth`:

```scala
  def runMinSc(cw: CountersWorld, m: Int, d: Int): Unit = {
    val name = cw.getClass.getName.split("[\\.\\$]").last
    print(s"\n$name ")
    val sc = new BigStepSс8[List[NW]] with CountersScWorld {
      val cnt: CountersWorld = cw
      val maxN: Int = m
      val maxDepth: Int = d
    }
    val l8 = sc.build_cograph(cw.start)
    val sl8 = sc.cl8_bad_conf(cw.isUnsafe)(l8)
    val sl = sc.prune(sl8)
    val (len_usl, size_usl) = size_unroll(sl)
    println(len_usl, size_usl)
    val ml = cl_min_size(sl)
    val mg = unroll(ml).head
    println(GraphPrettyPrinter.toString(mg))
  }
```

Then this functions generates a supercompiler `sc`, and builds a lazy cograph
`l8` that is cleaned with `cl8_bad_conf` to produced a new lazy cograph `sl8`,
containing only "safe" configurations. Then `prune` turns `sl8` into a (finite)
lazy graph `sl`, which is cleaned by `cl_min_size` to produce `ml`. `unroll(ml)`
returns a list containing a single graph of minimal size.
