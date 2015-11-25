package at.sevensuns.dertseha.reading.fp.ch08.testing

import at.sevensuns.dertseha.reading.fp.ch06.functionalstate.RNG
import at.sevensuns.dertseha.reading.fp.ch06.functionalstate.State

case class Gen[A]( sample: State[RNG, A] ) {

  // Exercise 8.6
  def flatMap[B]( f: A => Gen[B] ): Gen[B] =
    Gen( sample.flatMap( f( _ ).sample ) )

  def listOfN( size: Gen[Int] ): Gen[List[A]] =
    // I have nearly no idea what is happening here. I simply banged together some functions to match the signature...
    size.flatMap( count => flatMap( filler => Gen.unit( List.fill( count )( filler ) ) ) )
}

object Gen {

  // Exercise 8.4
  def choose( start: Int, stopExclusive: Int ): Gen[Int] =
    Gen( State( RNG.mapWithFlatMap( RNG.nonNegativeLessThan( stopExclusive - start ) )( _ + start ) ) )

  // Exercise 8.5
  def unit[A]( a: => A ): Gen[A] =
    //Gen( RNG.unit( a ) )   // Why doesn't this work? (What is missing)
    Gen( State( s => ( a, s ) ) )

  def boolean: Gen[Boolean] =
    Gen( State( RNG.mapWithFlatMap( RNG.nonNegativeLessThan( 2 ) )( _ == 1 ) ) )

  def listOfNManual[A]( n: Int, g: Gen[A] ): Gen[List[A]] = ???
  //Gen( State.ints( n ) )

}
