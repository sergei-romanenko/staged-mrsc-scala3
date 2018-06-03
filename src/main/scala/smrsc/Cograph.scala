package smrsc

//
// Infinite trees/graphs
//

//
// Lazy cographs of configurations
//

// A `LazyCograph[C]` represents a (potentially) infinite set of graphs
// of configurations (whose type is `Graph[C]`).
//
// "Lazy" cographs of configurations will be produced
// by the "lazy" (staged) version of multi-result
// supercompilation.

// LazyCoraph

sealed trait LazyCograph[+C]

case object Empty8 extends LazyCograph[Nothing]

case class Stop8[C](c: C) extends LazyCograph[C]

case class Build8[C](c: C, lss: () => List[List[LazyCograph[C]]])
  extends LazyCograph[C]

object LazyCograph {

  def empty8[C]: LazyCograph[C] = Empty8

  def stop8[C](c: C): LazyCograph[C] = Stop8(c)

  def build8[C](c: C, lss0: => List[List[LazyCograph[C]]]): LazyCograph[C] = {
    lazy val lss = lss0
    Build8(c, () => lss)
  }
}

trait BigStepSс8 extends ScWorld {

  import LazyCograph._

  // build_cograph

  def build_cograph_loop(h: History)(c: C): LazyCograph[C] =
    if (isFoldableToHistory(c, h))
      stop8(c)
    else {
      build8(c,
        develop(c).map(_.map(build_cograph_loop(c :: h))))
    }

  def build_cograph(c: C): LazyCograph[C] =
    build_cograph_loop(Nil)(c)

  // prune-cograph

  def prune_cograph_loop(h: History): LazyCograph[C] => LazyGraph[C] = {
    case Empty8 => Empty
    case Stop8(c) => Stop(c)
    case Build8(c, lss) =>
      if (isDangerous(h))
        Empty
      else
        Build(c,
          lss().map(_.map(prune_cograph_loop(c :: h))))
  }

  def prune_cograph(l : LazyCograph[C]): LazyGraph[C] =
    prune_cograph_loop(Nil)(l)

  //
  // Now that we have docomposed `lazy_mrsc`
  //     lazy_mrsc ≗ prune_cograph ∘ build_cograph
  // we can push some cleaners into prune_cograph.
  //
  // Suppose `clean∞` is a cograph cleaner such that
  //     clean ∘ prune_cograph ≗ prune_cograph ∘ clean∞
  // then
  //     clean ∘ lazy_mrsc ≗
  //       clean ∘ (prune_cograph ∘ build_cograph) ≗
  //       (prune_cograph ∘ clean∞) ∘ build_cograph
  //       prune_cograph ∘ (clean∞ ∘ build_cograph)
  //
  // The good thing is that `build_cograph` and `clean∞` work in a lazy way,
  // generating subtrees by demand. Hence, evaluating
  //     unroll( prune-cograph ∘ (clean∞ (build-cograph c)) )
  // may be less time and space consuming than evaluating
  //     unroll( clean (lazy-mrsc c) )
  //

  def cl8_bad_conf(bad: C => Boolean): LazyCograph[C] => LazyCograph[C] = {
    case Empty8 =>
      empty8
    case Stop8(c) =>
      if (bad(c)) empty8 else
        stop8(c)
    case Build8(c, lss) =>
      if (bad(c)) empty8 else
        build8(c, lss().map(_.map(cl8_bad_conf(bad))))
  }

  //
  // A cograph can be cleaned to remove some empty alternatives.
  //
  // Note that the cleaning is not perfect, because `cl∞-Ø` has to pass
  // the productivity check.
  // So, `build c []` is not (recursively) replaced with `Ø`. as is done
  // by `cl-empty`.
  //

  def cl8_empty: LazyCograph[C] => LazyCograph[C] = {
    case Empty8 =>
      empty8
    case Stop8(c) =>
      stop8(c)
    case Build8(c, lss) =>
      build8(c,
        lss()
          .filterNot(_.contains(Empty8))
          .map(_.map(cl8_empty)))
  }

  // An optimized version of `prune-cograph`.
  // The difference is that empty subtrees are removed
  // "on the fly".

  def prune_loop(h: History): LazyCograph[C] => LazyGraph[C] = {
    case Empty8 => Empty
    case Stop8(c) => Stop(c)
    case Build8(c, lss) =>
      if (isDangerous(h))
        Empty
      else
        Build(c,
          lss()
            .filterNot(_.contains(Empty8))
            .map(_.map(prune_loop(c :: h))))
  }

  def prune(l : LazyCograph[C]) : LazyGraph[C] =
    prune_loop(Nil)(l)

}
