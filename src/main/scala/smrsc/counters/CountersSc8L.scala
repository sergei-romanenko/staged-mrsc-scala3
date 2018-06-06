package smrsc.counters

import smrsc.Util.cartesian
import smrsc._

case class CountersSc8L(cnt: CountersWorld, maxN: Int, maxDepth: Int)
  extends BigStepSс8L {

  import cnt._

  type C = cnt.C

  private def isTooBig(c: C): Boolean =
    c.exists { case W => false case N(i) => i >= maxN }

  override def isDangerous(h: History): Boolean =
    h.exists(isTooBig)

  override def isFoldableTo(c1: C, c2: C): Boolean = {
    (c1, c2).zipped.forall { case (nw1, nw2) => nw1 isIn nw2 }
  }

  // Driving is deterministic
  def drive(c: C): List[C] =
    rules.flatMap(_.lift(c))

  // Rebuilding is not deterministic,
  // but makes a single configuration from a configuration.

  def rebuild1: NW => List[NW] = {
    case W => List(W)
    case N(i) => List[NW](i, W)
  }

  def rebuild(c: C): List[C] = {
    cartesian(c.map(rebuild1)).filterNot(_ == c)
  }

  override def develop(c: C): List[List[C]] =
    List(drive(c)) ::: rebuild(c).map(List(_))
}
