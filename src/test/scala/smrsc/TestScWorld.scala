package smrsc

trait TestScWorld extends ScWorld[Int] {

  type C = Int

  override def isDangerous(h: History): Boolean =
    h.length > 3

  override def isFoldableTo(c1: C, c2: C): Boolean =
    c1 == c2

  override def develop(c: C): List[List[C]] =
    drive(c) ::: rebuild(c)

  def drive(c: C): List[List[C]] =
    if (c < 2) List() else List(List(0, c - 1), List(c - 1))

  def rebuild(c: C): List[List[C]] =
    List(List(c + 1))
}
