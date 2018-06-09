package object smrsc {

  //
  // Cartesian product
  //

  def cartesian[A]: List[List[A]] => List[List[A]] =
    _.foldRight[List[List[A]]](List(Nil)) {
      case (xs, yss) =>
        xs.flatMap(x => yss.map(x :: _))
    }
}
