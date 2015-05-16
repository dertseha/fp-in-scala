package at.sevensuns.dertseha.reading.fp.ch04.handlingerrors

import org.junit._

@Test
class OptionTest {

  @Test
  def testVarianceOfEmptyListIsNone() = {
    val input: Seq[Double] = Nil
    val result = Option.variance( input )

    Assert.assertEquals( None, result )
  }

  @Test
  def testVarianceOfAListIsSome() = {
    val input = List( 1.0, 10.0, 1.0 )
    val result = Option.variance( input )

    Assert.assertEquals( Some( 18 ), result )
  }

  @Test
  def testMap2ReturnsNoneOnFirstNone() = {
    val result = Option.map2( None, Some( 2 ) )( math.max )

    Assert.assertEquals( None, result )
  }

  @Test
  def testMap2ReturnsNoneOnSecondNone() = {
    val result = Option.map2( Some( 1 ), None )( math.max )

    Assert.assertEquals( None, result )
  }

  @Test
  def testMap2ProducesResult() = {
    val result = Option.map2( Some( 1 ), Some( 2 ) )( math.max )

    Assert.assertEquals( Some( 2 ), result )
  }

  @Test
  def testSequenceReturnsEmptyListOnEmpty() = {
    val result = Option.sequence( List() )

    Assert.assertEquals( Some( List() ), result )
  }

  @Test
  def testSequenceReturnsNoneOnListOfOneNone() = {
    val result = Option.sequence( List( None ) )

    Assert.assertEquals( None, result )
  }

  @Test
  def testSequenceReturnsNoneOnListWithANone() {
    val result = Option.sequence( List( Some( "a" ), None ) )

    Assert.assertEquals( None, result )
  }

  @Test
  def testSequenceReturnsSomeOnListWithOnlySomes() {
    val result = Option.sequence( List( Some( "b" ), Some( "c" ) ) )

    Assert.assertEquals( Some( List( "b", "c" ) ), result )
  }

  @Test
  def testSequenceWithTraverseReturnsEmptyListOnEmpty() = {
    val result = Option.sequenceWithTraverse( List() )

    Assert.assertEquals( Some( List() ), result )
  }

  @Test
  def testSequenceWithTraverseReturnsNoneOnListOfOneNone() = {
    val result = Option.sequenceWithTraverse( List( None ) )

    Assert.assertEquals( None, result )
  }

  @Test
  def testSequenceWithTraverseReturnsNoneOnListWithANone() {
    val result = Option.sequenceWithTraverse( List( Some( "a" ), None ) )

    Assert.assertEquals( None, result )
  }

  @Test
  def testSequenceWithTraverseReturnsSomeOnListWithOnlySomes() {
    val result = Option.sequenceWithTraverse( List( Some( "b" ), Some( "c" ) ) )

    Assert.assertEquals( Some( List( "b", "c" ) ), result )
  }
}
