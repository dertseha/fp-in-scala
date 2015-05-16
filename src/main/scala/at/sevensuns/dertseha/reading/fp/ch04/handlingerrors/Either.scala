package at.sevensuns.dertseha.reading.fp.ch04.handlingerrors

sealed trait Either[+E, +A] {
  def map[B]( f: A => B ): Either[E, B]
  def flatMap[EE >: E, B]( f: A => Either[EE, B] ): Either[EE, B]
  def orElse[EE >: E, B >: A]( b: => Either[EE, B] ): Either[EE, B]
  def map2[EE >: E, B, C]( b: Either[EE, B] )( f: ( A, B ) => C ): Either[EE, C]
}

case class Left[+E]( value: E ) extends Either[E, Nothing] {
  def map[B]( f: Nothing => B ): Either[E, B] = this
  def flatMap[EE >: E, B]( f: Nothing => Either[EE, B] ): Either[EE, B] = this
  def orElse[EE >: E, B >: Nothing]( b: => Either[EE, B] ): Either[EE, B] = b
  def map2[EE >: E, B, C]( b: Either[EE, B] )( f: ( Nothing, B ) => C ): Either[EE, C] = this
}

case class Right[+A]( value: A ) extends Either[Nothing, A] {
  def map[B]( f: A => B ): Either[Nothing, B] =
    Right( f( value ) )

  def flatMap[EE >: Nothing, B]( f: A => Either[EE, B] ): Either[EE, B] =
    f( value )

  def orElse[EE >: Nothing, B >: A]( b: => Either[EE, B] ): Either[EE, B] =
    this

  def map2[EE >: Nothing, B, C]( b: Either[EE, B] )( f: ( A, B ) => C ): Either[EE, C] =
    b.map( f( value, _ ) )
}

object Either {
  def mean( xs: IndexedSeq[Double] ): Either[String, Double] =
    if ( xs.isEmpty )
      Left( "mean of empty list!" )
    else
      Right( xs.sum / xs.length )

  def Try[A]( a: => A ): Either[Exception, A] =
    try Right( a )
    catch { case e: Exception => Left( e ) }

  // Exercise 4.7
  def sequence[E, A]( es: List[Either[E, A]] ): Either[E, List[A]] =
    traverse( es )( identity )

  def traverse[E, A, B]( as: List[A] )( f: A => Either[E, B] ): Either[E, List[B]] =
    // the same implementation as Option.traverse(), just with a different init value for accumulator. Curious.
    as.foldRight( Right( List() ): Either[E, List[B]] )( ( in, acc ) => acc.flatMap( l => f( in ).map( _ :: l ) ) )
}
