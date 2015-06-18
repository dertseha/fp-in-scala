package at.sevensuns.dertseha.reading.fp.ch05.lazyness

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty        => None
    case Cons( h, t ) => Some( h() )
  }
  /* not compiling!
  def headOption: Option[A] = this match {
    case empty               => None
    case Stream.cons( h, t ) => Some( h() )
  }
  */

  def existsExplicit( p: A => Boolean ): Boolean = this match {
    case Cons( h, t ) => p( h() ) || t().existsExplicit( p )
    case _            => false
  }

  def exists( p: A => Boolean ): Boolean =
    foldRight( false )( ( a, b ) => p( a ) || b )

  def foldRight[B]( z: => B )( f: ( A, => B ) => B ): B =
    this match {
      case Cons( h, t ) => f( h(), t().foldRight( z )( f ) )
      case _            => z
    }

  // Exercise 5.1
  def toList: List[A] = this match {
    case Empty        => Nil
    case Cons( h, t ) => h() :: t().toList
  }

  // Exercise 5.2
  def take( n: Int ): List[A] = this match {
    case Cons( h, t ) if n > 0 => h() :: t().take( n - 1 )
    case _                     => Nil
  }

  def drop( n: Int ): List[A] = this match {
    case Cons( h, t ) if n > 0 => t().drop( n - 1 )
    case _                     => this.toList
  }

  // Exercise 5.3
  def takeWhile( p: A => Boolean ): Stream[A] = this match {
    case Cons( h, t ) if p( h() ) => Cons( h, () => t().takeWhile( p ) )
    case _                        => Empty
  }

  // Exercise 5.4
  def forAll( p: A => Boolean ): Boolean =
    foldRight( true )( ( elem, acc ) => p( elem ) && acc )

  // Exercise 5.5
  def takeWhileWithFoldRight( p: A => Boolean ): Stream[A] =
    foldRight( Empty: Stream[A] )( ( elem, acc ) => if ( p( elem ) ) Stream.cons( elem, acc ) else Empty )

  // Exercise 5.6
  def headOptionWithFoldRight: Option[A] =
    foldRight( None: Option[A] )( ( elem, acc ) => Some( elem ) )

  // Exercise 5.7
  def map[B]( f: A => B ): Stream[B] =
    foldRight( Empty: Stream[B] )( ( elem, acc ) => Stream.cons( f( elem ), acc ) )

  def filter( f: A => Boolean ): Stream[A] =
    foldRight( Empty: Stream[A] )( ( elem, acc ) => if ( f( elem ) ) Stream.cons( elem, acc ) else acc )

  def append[B >: A]( newTail: => Stream[B] ): Stream[B] =
    foldRight( newTail )( ( h, t ) => Stream.cons( h, t ) )

  def flatMap[B]( f: A => Stream[B] ): Stream[B] =
    foldRight( Empty: Stream[B] )( ( elem, acc ) => f( elem ).append( acc ) )

  // Exercise 5.13
  def unfoldMap[B]( f: A => B ): Stream[B] =
    Stream.unfold( () => this )( s => s() match {
      case Empty        => None
      case Cons( h, t ) => Some( ( f( h() ), t ) )
    } )

  def unfoldTake( n: Int ): List[A] =
    Stream.unfold( ( n, () => this ) )( s => {
      if ( s._1 > 0 ) {
        s._2() match {
          case Empty        => None
          case Cons( h, t ) => Some( ( h(), ( s._1 - 1, t ) ) )
        }
      } else None
    } ).toList

  def unfoldTakeWhile( p: A => Boolean ): Stream[A] = ???

  def zipAll[B]( s2: Stream[B] ): Stream[( Option[A], Option[B] )] = ???

  // Exercise 5.14
  def startsWith[B >: A]( start: Stream[B] ): Boolean = ???

  // Exercise 5.15
  def tails: Stream[Stream[A]] =
    Stream.unfold( () => this )( s => {
      lazy val ls = s()
      ls match {
        case Empty        => None
        case Cons( h, t ) => Some( ( ls, t ) )
      }
    } ).append( Stream( Empty ) )
}

case object Empty extends Stream[Nothing]
case class Cons[+A]( h: () => A, t: () => Stream[A] ) extends Stream[A] {
}

object Stream {
  def cons[A]( hd: => A, tl: => Stream[A] ): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons( () => head, () => tail )
  }

  def empty[A]: Stream[A] = Empty

  def apply[A]( as: A* ): Stream[A] =
    if ( as.isEmpty ) empty else cons( as.head, apply( as.tail: _* ) )

  val ones: Stream[Int] = Stream.cons( 1, ones )

  // Exercise 5.8
  def constant[A]( a: A ): Stream[A] = Stream.cons( a, constant( a ) )

  // Exercise 5.9
  def from( n: Int ): Stream[Int] = Stream.cons( n, from( n + 1 ) )

  // Exercise 5.10
  def fibs(): Stream[Int] = {
    def go( n1: Int, n2: Int ): Stream[Int] = Stream.cons( n1, go( n2, n1 + n2 ) )

    go( 0, 1 )
  }

  // Exercise 5.11
  def unfold[A, S]( z: S )( f: S => Option[( A, S )] ): Stream[A] = {
    /* causes stack overflow - keeping here for reference
    def go( cur: S ): Stream[A] = {
      val next = f( z )
      next.map( value => Stream.cons( value._1, go( value._2 ) ) ).getOrElse( empty )
    }

    go( z )
    */
    //Stream.cons(f(z))

    // Any way to prohibit immediate generation of first member?
    f( z ).map( value => Stream.cons( value._1, unfold( value._2 )( f ) ) ).getOrElse( empty )
  }

  // Exercise 5.12
  def unfoldFibs(): Stream[Int] = {
    unfold( ( 0, 1 ) )( s => Some( ( s._1, ( s._2, s._1 + s._2 ) ) ) )
  }

  def unfoldFrom( n: Int ): Stream[Int] = unfold( n )( s => Some( n, n + 1 ) )

  def unfoldConstant[A]( a: A ): Stream[A] = unfold( a )( s => Some( a, a ) )

  def unfoldOnes(): Stream[Int] = unfoldConstant( 1 )
}
