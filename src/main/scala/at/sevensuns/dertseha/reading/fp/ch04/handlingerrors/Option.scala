package at.sevensuns.dertseha.reading.fp.ch04.handlingerrors

sealed trait Option[+A] {
  def map[B]( f: A => B ): Option[B]
  def flatMap[B]( f: A => Option[B] ): Option[B]
  def getOrElse[B >: A]( default: => B ): B
  def orElse[B >: A]( ob: => Option[B] ): Option[B]
  def filter( f: A => Boolean ): Option[A]
}

case class Some[+A]( get: A ) extends Option[A] {
  def map[B]( f: A => B ): Option[B] =
    Some( f( get ) )

  def flatMap[B]( f: A => Option[B] ): Option[B] =
    f( get )

  def getOrElse[B >: A]( default: => B ): B =
    get

  def orElse[B >: A]( ob: => Option[B] ): Option[B] =
    this

  def filter( f: A => Boolean ): Option[A] =
    if ( f( get ) ) this else None

}
case object None extends Option[Nothing] {
  def map[B]( f: Nothing => B ): Option[B] = None
  def flatMap[B]( f: Nothing => Option[B] ): Option[B] = None
  def getOrElse[B >: Nothing]( default: => B ): B = default
  def orElse[B >: Nothing]( ob: => Option[B] ): Option[B] = ob
  def filter( f: Nothing => Boolean ): Option[Nothing] = None
}

object Option {
  def mean( xs: Seq[Double] ): Option[Double] =
    if ( xs.isEmpty ) None
    else Some( xs.sum / xs.length )

  // Exercise 4.2
  def variance( xs: Seq[Double] ): Option[Double] =
    mean( xs ).flatMap( m => mean( xs.map( x => math.pow( x - m, 2 ) ) ) )

  def lift[A, B]( f: A => B ): Option[A] => Option[B] = _ map f

  val absO: Option[Double] => Option[Double] = lift( math.abs )

  def Try[A]( a: => A ): Option[A] =
    try Some( a )
    catch { case e: Exception => None }

  // Exercise 4.3
  def map2[A, B, C]( a: Option[A], b: Option[B] )( f: ( A, B ) => C ): Option[C] =
    a.flatMap( av => b.map( bv => f( av, bv ) ) )

  // Exercise 4.4
  // Any practical difference between the two implementations? mapping on acc first avoids checking remaining X as soon
  // as acc is None - But the load is the same as still all Xs are iterated.
  def sequence[A]( a: List[Option[A]] ): Option[List[A]] =
    a.foldRight( Some( List() ): Option[List[A]] )( ( x, acc ) => acc.flatMap( l => x.map( xv => xv :: l ) ) )
  //a.foldRight( Some( List() ): Option[List[A]] )( ( x, acc ) => x.flatMap( xv => acc.map( l => xv :: l ) ) )

  // Exercise 4.5
  def traverse[A, B]( a: List[A] )( f: A => Option[B] ): Option[List[B]] =
    a.foldRight( Some( List() ): Option[List[B]] )( ( in, acc ) => acc.flatMap( l => f( in ).map( _ :: l ) ) )

  def sequenceWithTraverse[A]( a: List[Option[A]] ): Option[List[A]] =
    traverse( a )( identity )
}
