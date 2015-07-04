package at.sevensuns.dertseha.reading.fp.ch06.functionalstate

trait RNG {
  def nextInt: ( Int, RNG )
}

case class SimpleRNG( seed: Long ) extends RNG {
  def nextInt: ( Int, RNG ) = {
    val newSeed = ( seed * 0x5DEECE66DL + 0xBL ) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG( newSeed )
    val n = ( newSeed >>> 16 ).toInt
    ( n, nextRNG )
  }
}

object RNG {
  // Exercise 6.1
  def nonNegativeInt( rng: RNG ): ( Int, RNG ) = {
    val random = rng.nextInt

    ( if ( random._1 == Int.MinValue ) 0 else random._1 * Math.signum( random._1 ).toInt, random._2 )
  }

  // Exercise 6.2
  def double( rng: RNG ): ( Double, RNG ) = {
    val random = nonNegativeInt( rng )

    ( random._1.toDouble / ( Int.MaxValue.toDouble + 1 ), random._2 )
  }

  // Exercise 6.3
  def intDouble( rng: RNG ): ( ( Int, Double ), RNG ) = {
    val random1 = nonNegativeInt( rng )
    val random2 = double( random1._2 )

    ( ( random1._1, random2._1 ), random2._2 )
  }

  def doubleInt( rng: RNG ): ( ( Double, Int ), RNG ) = {
    val random1 = double( rng )
    val random2 = nonNegativeInt( random1._2 )

    ( ( random1._1, random2._1 ), random2._2 )
  }

  def double3( rng: RNG ): ( ( Double, Double, Double ), RNG ) = {
    val random1 = double( rng )
    val random2 = double( random1._2 )
    val random3 = double( random2._2 )

    ( ( random1._1, random2._1, random3._1 ), random3._2 )
  }

  // Exercise 6.4
  def ints( count: Int )( rng: RNG ): ( List[Int], RNG ) = {
    if ( count > 0 ) {
      val next = rng.nextInt
      val stage = ints( count - 1 )( next._2 )
      ( next._1 :: stage._1, stage._2 )
    } else {
      ( Nil, rng )
    }
  }

  type Rand[+A] = RNG => ( A, RNG )

  val int: Rand[Int] = _.nextInt

  def unit[A]( a: A ): Rand[A] = rng => ( a, rng )

  def map[A, B]( s: Rand[A] )( f: A => B ): Rand[B] =
    rng => {
      val ( a, rng2 ) = s( rng )
      ( f( a ), rng2 )
    }

  def nonNegativeEven: Rand[Int] =
    map( nonNegativeInt )( i => i - i % 2 )

  // Exercise 6.5
  def doubleWithMap( rng: RNG ): Rand[Double] =
    map( nonNegativeInt )( i => i.toDouble / ( Int.MaxValue.toDouble + 1 ) )

  // Exercise 6.6
  // note: funny I was already thinking "I need a flatMap here" :)
  def map2[A, B, C]( ra: Rand[A], rb: Rand[B] )( f: ( A, B ) => C ): Rand[C] =
    rng => {
      val ( a, rng2 ) = ra( rng )
      val ( b, rng3 ) = rb( rng2 )

      ( f( a, b ), rng3 )
    }

  def both[A, B]( ra: Rand[A], rb: Rand[B] ): Rand[( A, B )] =
    map2( ra, rb )( ( _, _ ) )

  val randIntDoubleWithMap: Rand[( Int, Double )] =
    both( int, double )
  val randDoubleIntWithMap: Rand[( Double, Int )] =
    both( double, int )

  // Exercise 6.7
  def sequence[A]( fs: List[Rand[A]] ): Rand[List[A]] =
    rng => fs.foldLeft( ( Nil: List[A], rng ) )( ( acc, elem ) => {
      val ( result, rng2 ) = elem( acc._2 )
      ( result :: acc._1, rng2 )
    } )

  def intsWithSequence( count: Int ): Rand[List[Int]] =
    sequence( List.fill( count )( rng => rng.nextInt ) )

  // Exercise 6.8
  def flatMap[A, B]( f: Rand[A] )( g: A => Rand[B] ): Rand[B] =
    rng => {
      val ( a, rng2 ) = f( rng )
      g( a )( rng2 )
    }

  def nonNegativeLessThan( n: Int ): Rand[Int] =
    flatMap( nonNegativeInt )( value => rng => ( value % n, rng ) )

  // Exercise 6.9
  def mapWithFlatMap[A, B]( s: Rand[A] )( f: A => B ): Rand[B] =
    flatMap( s )( a => rng => ( f( a ), rng ) )

  def map2WithFlatMap[A, B, C]( ra: Rand[A], rb: Rand[B] )( f: ( A, B ) => C ): Rand[C] =
    flatMap( ra )( a => flatMap( rb )( b => rng => ( f( a, b ), rng ) ) )

  def rollDie: Rand[Int] = mapWithFlatMap( nonNegativeLessThan( 6 ) )( _ + 1 )
}
