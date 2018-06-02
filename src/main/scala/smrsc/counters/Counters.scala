package smrsc.counters

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

  //override def toString = "ω"
}

trait CountersWorld {

  import scala.language.implicitConversions

  implicit def int2NOmega(i: Int): NW = N(i)

  type C = List[NW]

  type Rule = PartialFunction[C, C]
  val start: C
  val rules: List[Rule]
  val isUnsafe : C => Boolean
}
