package at.sevensuns.dertseha.reading.fp.ch11.monads

import at.sevensuns.dertseha.reading.fp.ch08.testing.Gen
import at.sevensuns.dertseha.reading.fp.ch04.handlingerrors.Option
import at.sevensuns.dertseha.reading.fp.ch04.handlingerrors.Some
import at.sevensuns.dertseha.reading.fp.ch06.functionalstate.State

trait Monad[F[_]] extends Functor[F] {
  def unit[A]( a: => A ): F[A]
  def flatMap[A, B]( ma: F[A] )( f: A => F[B] ): F[B]

  def map[A, B]( ma: F[A] )( f: A => B ): F[B] =
    flatMap( ma )( a => unit( f( a ) ) )
  def map2[A, B, C]( ma: F[A], mb: F[B] )( f: ( A, B ) => C ): F[C] =
    flatMap( ma )( a => map( mb )( b => f( a, b ) ) )

  def product[A, B]( ma: F[A], mb: F[B] ): F[( A, B )] = map2( ma, mb )( ( _, _ ) )

  // Exercise 11.3
  // Note: I forgot how they work, only copied from other implementation and matched types...
  def sequence[A]( lma: List[F[A]] ): F[List[A]] =
    lma.foldRight( unit( Nil: List[A] ) )( ( x, acc ) => map2( x, acc )( _ :: _ ) )
  //lma.foldRight( unit( Nil: List[A] ) )( ( x, acc ) => flatMap( acc )( l => map( x )( xv => xv :: l ) ) )

  def traverse[A, B]( la: List[A] )( f: A => F[B] ): F[List[B]] = {
    sequence( la.map( f ) )
  }

  // Exercise 11.4
  def replicateOur[A]( n: Int, ma: F[A] ): F[List[A]] = {
    map( ma )( a => List.fill( n )( a ) )
  }

  def replicateM[A]( n: Int, ma: F[A] ): F[List[A]] = {
    sequence( List.fill( n )( ma ) )
  }

  def filterMOfficial[A]( ms: List[A] )( f: A => F[Boolean] ): F[List[A]] =
    ms.foldRight( unit( List[A]() ) )( ( x, y ) =>
      compose( f, ( b: Boolean ) => if ( b ) map2( unit( x ), y )( _ :: _ ) else y )( x ) )

  def filterM[A]( ms: List[A] )( f: A => F[Boolean] ): F[List[A]] = {

    ms.foldRight( unit( Nil: List[A] ) )( ( x, acc ) =>
      map2( f( x ), acc )( ( b, l ) => if ( b ) x :: l else l ) )
  }

  // Exercise 11.5
  // Too late to think :)

  // Exercise 11.6
  // Only followed the types. I have no idea if this works as intended.
  def filterMMy[A]( ms: List[A] )( f: A => F[Boolean] ): F[List[A]] = {
    val results = sequence( ms.map( a => f( a ) ) )
    val joined = map( results )( l => l.zip( ms ) )

    map( joined )( list => list.foldRight( Nil: List[A] )( ( tuple, acc ) => if ( tuple._1 ) tuple._2 :: acc else acc ) )
  }

  // Exercise 11.7
  def compose[A, B, C]( f: A => F[B], g: B => F[C] ): A => F[C] =
    a => flatMap( f( a ) )( g )

  // Exercise 11.8
  def flatMapWithCompose[A, B]( ma: F[A] )( f: A => F[B] ): F[B] = {
    //compose( f, map( ma )( unit ) )
    //val composed = compose( f, ( b: B ) => unit( b ) )
    //map( ma )( a => map( composed )( b => b ) )
    ???
  }

  // Exercise 11.9 - 11.11 -- skipped

  // Exercise 11.12
  def join[A]( mma: F[F[A]] ): F[A] = ???

  // Exercise 11.13
  def flatMapWithJoinAndMap[A, B]( ma: F[A] )( f: A => F[B] ): F[B] = ???
  def flatComposeWithJoinAndMap[A, B]( ma: F[A] )( f: A => F[B] ): F[B] = ???

  // Exercise 11.14 - 11.16 -- skipped

}

object Monad {

  val genMonad = new Monad[Gen] {
    def unit[A]( a: => A ): Gen[A] = Gen.unit( a )
    def flatMap[A, B]( ma: Gen[A] )( f: A => Gen[B] ): Gen[B] =
      ma flatMap f
  }

  // Exercise 11.1
  val optionMonad = new Monad[Option] {

    def unit[A]( a: => A ): Option[A] = Some( a )
    def flatMap[A, B]( ma: Option[A] )( f: A => Option[B] ): Option[B] = ma flatMap f
  }

  val listMonad = new Monad[List] {

    def unit[A]( a: => A ): List[A] = List( a )
    def flatMap[A, B]( ma: List[A] )( f: A => List[B] ): List[B] = ma flatMap f
  }

  // Exercise 11.2
  // Thought experiment: Tuple? Nope.

  // Exercise 11.17
  val idMonad = new Monad[Id] {
    def unit[A]( a: => A ): Id[A] = Id( a )
    def flatMap[A, B]( ma: Id[A] )( f: A => Id[B] ): Id[B] = ma flatMap f
  }

  def stateMonad[S] = new Monad[( { type f[x] = State[S, x] } )#f] {
    def unit[A]( a: => A ): State[S, A] = State( s => ( a, s ) )
    def flatMap[A, B]( st: State[S, A] )( f: A => State[S, B] ): State[S, B] =
      st flatMap f
  }
}
