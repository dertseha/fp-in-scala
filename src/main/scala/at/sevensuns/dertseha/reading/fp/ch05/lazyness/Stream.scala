package at.sevensuns.dertseha.reading.fp.ch05.lazyness

sealed trait Stream[+A] {
  /* not compiling!
  def headOption: Option[A] = this match {
    case empty               => None
    case Stream.cons( h, t ) => Some( h() )
  }
  */

  // Exercise 5.1
  def toList: List[A] = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A]( h: () => A, t: () => Stream[A] ) extends Stream[A]

object Stream {
  def cons[A]( hd: => A, tl: => Stream[A] ): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons( () => head, () => tail )
  }

  def empty[A]: Stream[A] = Empty

  def apply[A]( as: A* ): Stream[A] =
    if ( as.isEmpty ) empty else cons( as.head, apply( as.tail: _* ) )

}
