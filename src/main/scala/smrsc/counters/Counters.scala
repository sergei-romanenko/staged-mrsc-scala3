package smrsc.counters

import smrsc.{cartesian, ScWorld}

sealed trait NW {
  def +(comp: NW): NW

  def -(comp: NW): NW

  def >=(i: Int): Boolean

  def ===(i: Int): Boolean

  def isIn(nw: NW): Boolean
}

case class N(i: Int) extends NW {
  def +(nw: NW): NW =
    nw match {
      case W => W
      case N(j) => N(i + j)
    }

  def -(nw: NW): NW =
    nw match {
      case W => W
      case N(j) => N(i - j)
    }

  def >=(j: Int): Boolean =
    i >= j

  def ===(j: Int): Boolean =
    i == j

  def isIn(nw: NW): Boolean =
    nw match {
      case W => true
      case N(j) => i == j
    }

  override def toString: String =
    i.toString
}

case object W extends NW {
  def +(nw: NW): W.type = W

  def -(nw: NW): W.type = W

  def >=(i: Int) = true

  def ===(i: Int) = true

  def isIn(nw: NW): Boolean = W == nw

  override def toString = "ω"
}

trait CountersWorld {

  import scala.language.implicitConversions

  implicit def int2NW(i: Int): NW = N(i)

  type C = List[NW]

  type Rule = PartialFunction[C, C]
  val start: C
  val rules: List[Rule]
  val isUnsafe: C => Boolean

}

trait CountersScWorld extends ScWorld[List[NW]] {

  val cnt: CountersWorld
  val maxN: Int
  val maxDepth: Int

  import cnt._

  private def isTooBig(c: C): Boolean =
    c.exists { case W => false case N(i) => i >= maxN }

  override def isDangerous(h: History): Boolean =
    h.exists(isTooBig) || h.length >= maxDepth

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
