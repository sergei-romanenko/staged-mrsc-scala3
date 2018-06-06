package smrsc

object Util {

  //
  // Cartesian product
  //

  def cartesian2[A]: (List[A], List[List[A]]) => List[List[A]] = {
    case (Nil, yss) => Nil
    case (x :: xs, yss) =>
      yss.map(x :: _) ++ cartesian2(xs, yss)
  }

  def cartesian[A]: List[List[A]] => List[List[A]] = {
    case Nil => List(Nil)
    case xs :: xss =>
      cartesian2(xs, cartesian(xss))
  }

}
