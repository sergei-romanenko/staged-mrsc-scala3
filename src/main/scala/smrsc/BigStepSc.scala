package smrsc

// ### Schemes of different types of big-step supercompilation

// A variation of the scheme presented in the paper
//
// Ilya G. Klyuchnikov, Sergei A. Romanenko. Formalizing and Implementing
// Multi-Result Supercompilation.
// In Third International Valentin Turchin Workshop on Metacomputation
// (Proceedings of the Third International Valentin Turchin Workshop on
// Metacomputation. Pereslavl-Zalessky, Russia, July 5-9, 2012).
// A.V. Klimov and S.A. Romanenko, Ed. - Pereslavl-Zalessky: Ailamazyan
// University of Pereslavl, 2012, 260 p. ISBN 978-5-901795-28-6, pages
// 142-164.

// Now we formulate an idealized model of big-step multi-result
// supercompilation.
//
// The knowledge about the input language a supercompiler deals with
// is represented by a "world of supercompilation", which is a trait
// that specifies the following.
//
// * `C` is the type of "configurations". Note that configurations are
//   not required to be just expressions with free variables! In general,
//   they may represent sets of states in any form/language and as well may
//   contain any _additional_ information.
//
// * `isFoldableTo` is a "foldability relation". isFoldableTo(c, c') means
//   that c is foldable to c'.
//   (In such cases c' is usually said to be " more general than c".)
//
// * `develop` is a function that gives a number of possible decompositions of
//   a configuration. Let `c` be a configuration and `cs` a list of
//   configurations such that `cs ∈ develop(c)`. Then `c` can be "reduced to"
//   (or "decomposed into") configurations in `cs`.
//
//   Suppose that driving is determinstic and, given a configuration `c`,
//   produces a list of configurations `drive(c)`. Suppose that rebuilding
//   (generalization, application of lemmas) is non-deterministic and
//   `rebuild(c)` is the list of configurations that can be produced by
//   rebuilding. Then (in this special case) `develop` is implemented
//   as follows:
//
//       develop(c) = List(drive(c)) ::: rebuild(c).map(List(_))
//
// * `History` is a list of configuration that have been produced
//   in order to reach the current configuration.
//
// * `isDangerous` is a "whistle" that is used to ensure termination of
//   supercompilation. `isDangerous(h)` means that the history has become
//   "too large".
//
// * `isFoldableToHistory(c, h)` means that `c` is foldable to a configuration
//   in the history `h`.

trait ScWorld[C]:

  type History = List[C]

  def isDangerous(h: History): Boolean

  def isFoldableTo(c1: C, c2: C): Boolean

  def develop(c: C): List[List[C]]

  def isFoldableToHistory(c: C, h: History): Boolean =
    h.exists(isFoldableTo(c, _))

trait BigStepSc[C]:

  this: ScWorld[C] =>

  //Big-step multi-result supercompilation
  // (The naive version builds Cartesian products immediately.)

  def naive_mrsc_loop(h: History)(c: C): List[Graph[C]] =
    if isFoldableToHistory(c, h) then
      List(Back(c))
    else if isDangerous(h) then
      List()
    else
      develop(c)
        .flatMap(cs => cartesian(cs.map(naive_mrsc_loop(c :: h))))
        .map(Forth(c, _))

  def naive_mrsc(c: C): List[Graph[C]] =
    naive_mrsc_loop(Nil)(c)

  // "Lazy" multi-result supercompilation.
  // (Cartesian products are not immediately built.)
  //
  // lazy_mrsc is essentially a "staged" version of naive-mrsc
  // with get-graphs being an "interpreter" that evaluates the "program"
  // returned by lazy_mrsc.

  def lazy_mrsc_loop(h: History)(c: C): LazyGraph[C] =
    if isFoldableToHistory(c, h) then
      Stop(c)
    else if isDangerous(h) then
      Empty
    else
      Build(c,
        develop(c).map(_.map(lazy_mrsc_loop(c :: h))))

  def lazy_mrsc(c: C): LazyGraph[C] =
    lazy_mrsc_loop(Nil)(c)

