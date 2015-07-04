package at.sevensuns.dertseha.reading.fp.ch06.functionalstate

import org.junit._

@Test
class RNGTest {

  @Test
  def testNonNegativeIntReturnsValue() = {
    val rng = SimpleRNG( 42 )
    val result = RNG.nonNegativeInt( rng )

    Assert.assertEquals( 16159453, result._1 )
  }

  @Test
  def testIntsWithSequence() = {
    val rng = SimpleRNG( 42 )
    val result = RNG.intsWithSequence( 3 )

    Assert.assertEquals( ( List( -340305902, -1281479697, 16159453 ), SimpleRNG( 259172689157871L ) ), result( rng ) )
  }

  @Test
  def testRollDie() = {
    def go( remain: Int, rng: RNG ) {
      if ( remain > 0 ) {
        val ( result, rng2 ) = RNG.rollDie( rng )

        Assert.assertTrue( result >= 1 && result <= 6 )
        go( remain - 1, rng2 )
      }
    }

    go( 1000, SimpleRNG( 42 ) )
  }
}
