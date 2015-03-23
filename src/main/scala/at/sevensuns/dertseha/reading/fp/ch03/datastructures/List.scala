package at.sevensuns.dertseha.reading.fp.ch03.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A]( head: A, tail: List[A] ) extends List[A]

object List {
  def sum( ints: List[Int] ): Int = ints match {
    case Nil           => 0
    case Cons( x, xs ) => x + sum( xs )
  }

  def product( ds: List[Double] ): Double = ds match {
    case Nil            => 1.0
    case Cons( 0.0, _ ) => 0.0
    case Cons( x, xs )  => x * product( xs )
  }

  def apply[A]( as: A* ): List[A] =
    if ( as.isEmpty ) Nil
    else Cons( as.head, apply( as.tail: _* ) )

  def append[A]( a1: List[A], a2: List[A] ): List[A] =
    a1 match {
      case Nil          => a2
      case Cons( h, t ) => Cons( h, append( t, a2 ) )
    }

  def foldRight[A, B]( as: List[A], z: B )( f: ( A, B ) => B ): B =
    as match {
      case Nil           => z
      case Cons( x, xs ) => f( x, foldRight( xs, z )( f ) )
    }

  def sum2( ns: List[Int] ) =
    foldRight( ns, 0 )( ( x, y ) => x + y )

  def product2( ns: List[Double] ) =
    foldRight( ns, 1.0 )( _ * _ )

  // Exercise 3.2
  def tail[A]( list: List[A] ): List[A] = list match {
    case Nil           => Nil
    case Cons( x, xs ) => xs
  }

  // Exercise 3.3
  def setHead[A]( list: List[A], head: A ): List[A] = list match {
    case Nil           => Cons( head, Nil )
    case Cons( _, xs ) => Cons( head, xs )
  }

  // Exercise 3.4
  def drop[A]( l: List[A], n: Int ): List[A] =
    if ( n == 0 ) l
    else l match {
      case Nil           => Nil
      case Cons( _, xs ) => drop( xs, n - 1 )
    }

  // Exercise 3.5
  def dropWhile[A]( l: List[A] )( f: A => Boolean ): List[A] = l match {
    case Nil           => Nil
    case Cons( x, xs ) => if ( f( x ) ) dropWhile( xs )( f ) else l
  }

  // Exercise 3.6
  def init[A]( l: List[A] ): List[A] = l match {
    case Nil            => Nil
    case Cons( _, Nil ) => Nil
    case Cons( x, xs )  => Cons( x, init( xs ) )
  }

  // Exercise 3.9
  def length[A]( as: List[A] ): Int =
    foldRight( as, 0 )( ( _, acc ) => acc + 1 )

  // Exercise 3.10
  @annotation.tailrec
  def foldLeft[A, B]( as: List[A], z: B )( f: ( B, A ) => B ): B =
    as match {
      case Nil           => z
      case Cons( x, xs ) => foldLeft( xs, f( z, x ) )( f )
    }

  // Exercise 3.11
  def sumFoldLeft( ns: List[Int] ) =
    foldLeft( ns, 0 )( _ + _ )

  def productFoldLeft( ns: List[Double] ) =
    foldLeft( ns, 1.0 )( _ * _ )

  def lengthFoldLeft[A]( as: List[A] ) =
    foldLeft( as, 0 )( ( acc, _ ) => acc + 1 )

  // Exercise 3.12
  def reverse[A]( as: List[A] ): List[A] =
    foldLeft( as, Nil: List[A] )( ( acc, x ) => Cons( x, acc ) )

  // Exercise 3.13
  def foldLeftViaFoldRight[A, B]( as: List[A], z: B )( f: ( B, A ) => B ): B = {
    foldRight( as, ( acc: B ) => acc )( ( x, acc ) => ( prev: B ) => acc( f( prev, x ) ) )( z )
  }

  def foldRightViaFoldLeft[A, B]( as: List[A], z: B )( f: ( A, B ) => B ): B =
    foldLeft( reverse( as ), z )( ( acc, x ) => f( x, acc ) )

  // Exercise 3.14
  def appendFoldLeft[A]( a1: List[A], a2: List[A] ): List[A] =
    foldLeft( reverse( a1 ), a2 )( ( acc, x ) => Cons( x, acc ) )

  def appendFoldRight[A]( a1: List[A], a2: List[A] ): List[A] =
    foldRight( a1, a2 )( ( x, acc ) => Cons( x, acc ) )

  // Exercise 3.15
  def flattenList[A]( ll: List[List[A]] ): List[A] =
    foldRight( ll, Nil: List[A] )( ( x, acc ) => foldRight( x, acc )( Cons( _, _ ) ) )

  // Exercise 3.16
  def addOneToList( list: List[Int] ): List[Int] =
    foldRight( list, Nil: List[Int] )( ( x, acc ) => Cons( x + 1, acc ) )

  // Exercise 3.17
  def doubleToString( list: List[Double] ): List[String] =
    foldRight( list, Nil: List[String] )( ( x, acc ) => Cons( x.toString(), acc ) )

  // Exercise 3.18
  def map[A, B]( as: List[A] )( f: A => B ): List[B] =
    foldRight( as, Nil: List[B] )( ( x, acc ) => Cons( f( x ), acc ) )

  // Exercise 3.19
  def filter[A]( as: List[A] )( f: A => Boolean ): List[A] =
    foldRight( as, Nil: List[A] )( ( x, acc ) => if ( f( x ) ) Cons( x, acc ) else acc )

  // Exercise 3.20
  def flatMap[A, B]( as: List[A] )( f: A => List[B] ): List[B] =
    foldRight( as, Nil: List[B] )( ( x, acc ) => foldRight( f( x ), acc )( Cons( _, _ ) ) )

  // Exercise 3.21
  def filterViaFlatMap[A]( as: List[A] )( f: A => Boolean ): List[A] = ???
}
