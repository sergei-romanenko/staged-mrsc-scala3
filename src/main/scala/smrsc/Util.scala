package smrsc

import scala.collection.immutable

object Util {

  //
  // Cartesian product
  //

  // cartesian2

  def cartesian2[A]: (List[A], List[List[A]]) => List[List[A]] = {
    case (Nil, yss) => Nil
    case (x :: xs, yss) =>
      yss.map(x :: _) ++ cartesian2(xs, yss)
  }

  // cartesian

  def cartesian[A]: List[List[A]] => List[List[A]] = {
    case Nil => List(Nil)
    case xs :: xss =>
      cartesian2(xs, cartesian(xss))
  }

}
