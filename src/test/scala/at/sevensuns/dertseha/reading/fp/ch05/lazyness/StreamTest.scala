package at.sevensuns.dertseha.reading.fp.ch05.lazyness

import org.junit._

@Test
class StreamTest {

  @Test
  def testToListConvertsToList() = {
    val input = Stream( 1, 2, 3 )
    val result = input.toList

    Assert.assertEquals( List( 1, 2, 3 ), result )
  }

  @Test
  def testTakeNReturnsNElements() = {
    val input = Stream( 1, 2, 3, 4 )
    val result = input.take( 2 )

    Assert.assertEquals( List( 1, 2 ), result )
  }

  @Test
  def testDropNSkipsNElements() = {
    val input = Stream( 1, 2, 3, 4, 5 )
    val result = input.drop( 3 )

    Assert.assertEquals( List( 4, 5 ), result )
  }

  @Test
  def testTakeWhileReturnsFirstElements() = {
    val input = Stream( 1, 2, 3, 4, 3, 2, 1 )
    val result = input.takeWhile( _ < 3 ).toList

    Assert.assertEquals( List( 1, 2 ), result )
  }

  @Test
  def testForAllReturnsTrueIfMatched() = {
    val input = Stream( 2, 4, 6, 8 )
    val result = input.forAll( _ % 2 == 0 )

    Assert.assertTrue( result )
  }

  @Test
  def testForAllReturnsFalseForNonmatch() = {
    val input = Stream( 2, 4, 5, 8 )
    val result = input.forAll( _ % 2 == 0 )

    Assert.assertFalse( result )
  }

  @Test
  def testHeadOptionWithFoldRightReturnsNoneForEmpty() {
    Assert.assertEquals( Empty.headOptionWithFoldRight, None )
  }

  @Test
  def testHeadOptionWithFoldRightReturnsSomeForNonEmpty() {
    val input = Stream( 2, 4, 5, 8 )
    val result = input.headOptionWithFoldRight

    Assert.assertEquals( result, Some( 2 ) )
  }

  @Test
  def testMap() {
    val input = Stream( 1, 2, 3, 4 )
    val result = input.map( _.toString() )

    Assert.assertEquals( List( "1", "2", "3", "4" ), result.toList )
  }

  @Test
  def testFilter() {
    val input = Stream( 1, 2, 3, 4 )
    val result = input.filter( _ % 2 == 0 )

    Assert.assertEquals( List( 2, 4 ), result.toList )
  }

  @Test
  def testFlatMap() {
    val input = Stream( 1, 2, 3, 4 )
    val result = input.flatMap( elem => Stream( elem.toString() ) )

    Assert.assertEquals( List( "1", "2", "3", "4" ), result.toList )
  }

  @Test
  def testFibs() {
    val result = Stream.fibs().take( 5 )

    Assert.assertEquals( List( 0, 1, 1, 2, 3 ), result.toList )
  }

  @Test
  def testUnfold() {
    val result = Stream.unfold( 1 )( state => if ( state < 6 ) Some( ( state.toString(), state + 1 ) ) else None )

    Assert.assertEquals( List( "1", "2", "3", "4", "5" ), result.toList )
  }

  @Test
  @Ignore
  def testStartsWithReturnsFalseIfEmpty() {
    val result = Empty.startsWith( Stream( 1, 2, 3 ) )

    Assert.assertFalse( result )
  }

  @Test
  @Ignore
  def testStartsWithReturnsTrueIfMatched() {
    val result = Stream( 1, 2, 3, 4 ).startsWith( Stream( 1, 2, 3 ) )

    Assert.assertTrue( result )
  }

  @Test
  @Ignore
  def testStartsWithReturnsFalseForWrongData() {
    val result = Stream( 2, 3, 4, 5 ).startsWith( Stream( 1, 2, 3 ) )

    Assert.assertFalse( result )
  }

  @Test
  @Ignore
  def testStartsWithReturnsFalseForShortData() {
    val result = Stream( 1, 2 ).startsWith( Stream( 1, 2, 3 ) )

    Assert.assertFalse( result )
  }

  @Test
  def testTails() {
    val result = Stream( 1, 2, 3 ).tails

    Assert.assertEquals( Stream( Stream( 1, 2, 3 ).toList, Stream( 2, 3 ).toList, Stream( 3 ).toList,
      Stream().toList ).toList, result.map( _.toList ).toList )
  }
}
