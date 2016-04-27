package at.sevensuns.dertseha.reading.fp.ch08.testing

import at.sevensuns.dertseha.reading.fp.ch06.functionalstate.RNG
import at.sevensuns.dertseha.reading.fp.ch06.functionalstate.State

case class Gen[A]( sample: State[RNG, A] ) {

  // Exercise 8.6
  def flatMap[B]( f: A => Gen[B] ): Gen[B] =
    Gen( sample.flatMap( f( _ ).sample ) )

  def listOfN( size: Gen[Int] ): Gen[List[A]] =
    size.flatMap( count => Gen.listOfNManual( count, this ) )
}

object Gen {

  // Exercise 8.4
  def choose( start: Int, stopExclusive: Int ): Gen[Int] =
    Gen( State( RNG.mapWithFlatMap( RNG.nonNegativeLessThan( stopExclusive - start ) )( _ + start ) ) )
  // Gen( State( RNG.nonNegativeLessThan( stopExclusive - start ) ).map( _ + start ) )
  // Gen( State( RNG.map( RNG.nonNegativeLessThan( stopExclusive - start ) )( _ + start ) ) )

  // Exercise 8.5
  def unit[A]( a: => A ): Gen[A] =
    // Gen( State( RNG.unit( a ) ) )
    Gen( State.unit( a ) )

  def boolean: Gen[Boolean] =
    // Gen( State( RNG.mapWithFlatMap( RNG.nonNegativeLessThan( 2 ) )( _ == 1 ) ) )
    Gen( State( RNG.int ).map( _ % 2 == 0 ) )

  def listOfNManual[A]( n: Int, g: Gen[A] ): Gen[List[A]] =
    Gen( State.sequence( List.fill( n )( g.sample ) ) )

  // Exercise 8.7
  def union[A]( g1: Gen[A], g2: Gen[A] ): Gen[A] =
    boolean flatMap ( takeLeft => if ( takeLeft ) g1 else g2 )
}
