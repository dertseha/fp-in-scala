package at.sevensuns.dertseha.reading.fp.ch03.datastructures

import org.junit._

@Test
class MyModuleTest {

  @Test
  def testTailReturnsTailOfList() = {
    val tail = List( 1, 2, 3 )
    val full = Cons( 0, tail )
    val result = List.tail( full )

    Assert.assertEquals( tail, result )
  }

  @Test
  def testSetHeadReturnsNewList() = {
    val tail = List( 1, 2, 3 )
    val first = Cons( 0, tail )
    val result = List.setHead( first, -1 )

    Assert.assertEquals( Cons( -1, tail ), result )
  }

  @Test
  def testDropRemovesFirstElements() = {
    val input = List( 1, 2, 3, 4 )
    val result = List.drop( input, 3 )

    Assert.assertEquals( List( 4 ), result )
  }

  @Test
  def testDropWhileRemovesFirstElementsForPredicate() = {
    val input = List( 4, 3, 2, 1 )
    val result = List.dropWhile( input )( _ > 2 )

    Assert.assertEquals( List( 2, 1 ), result )
  }

  @Test
  def testInitRemovesLastElement() = {
    val input = List( 4, 3, 2, 1 )
    val result = List.init( input )

    Assert.assertEquals( List( 4, 3, 2 ), result )
  }

  @Test
  def testInitOfNilIsNil() = {
    Assert.assertEquals( Nil, List.init( Nil ) )
  }

  @Test
  def testInitOfSingleElementIsNil() = {
    Assert.assertEquals( Nil, List.init( List( 1 ) ) )
  }

  @Test
  def testLengthReturnsLengthOfList() = {
    Assert.assertEquals( 4, List.length( List( "A", "B", "C", "D" ) ) )
  }

  def largeList(): List[Int] = {
    var i: Int = 0
    var base: List[Int] = Nil

    while ( i < 10000 ) {
      base = Cons( i, base )
      i = i + 1
    }

    return base
  }

  @Test
  def testFoldRightCausesStackOverflow() = {
    try {
      List.foldRight( largeList(), 0 )( ( _, acc ) => acc )
      Assert.fail( "Expected Stack Overflow" )
    } catch {
      case _: StackOverflowError => ()
    }
  }

  @Test
  def testFoldLeftWorks() = {
    val result = List.foldLeft( List( "A", "B", "C" ), "" )( ( acc, x ) => acc + x )

    Assert.assertEquals( "ABC", result )
  }

  @Test
  def testReverseReturnsReversedList() = {
    val input = List( 1, 2, 3, 4 )
    val result = List.reverse( input )

    Assert.assertEquals( List( 4, 3, 2, 1 ), result )
  }

  @Test
  def testFoldLeftViaFoldRightWorks() = {
    val input = List( "A", "B", "C" )
    val result = List.foldLeftViaFoldRight( input, "" )( ( acc, x ) => acc + x )

    Assert.assertEquals( "ABC", result )
  }

  @Test
  def testFoldRightViaFoldLeftWorks() = {
    val input = List( "A", "B", "C" )
    val result = List.foldRightViaFoldLeft( input, "" )( ( x, acc ) => x + acc )

    Assert.assertEquals( "ABC", result )
  }

  @Test
  def testAppendWithFoldLeft() = {
    val result = List.appendFoldLeft( List( 1, 2, 3 ), List( 4, 5, 6 ) )

    Assert.assertEquals( List( 1, 2, 3, 4, 5, 6 ), result )
  }

  @Test
  def testAppendWithFoldRight() = {
    val result = List.appendFoldRight( List( 1, 2, 3 ), List( 4, 5, 6 ) )

    Assert.assertEquals( List( 1, 2, 3, 4, 5, 6 ), result )
  }

  @Test
  def testFlattenList() = {
    val result = List.flattenList( List( List( 1, 2 ), List( 3, 4 ), List( 5, 6 ) ) )

    Assert.assertEquals( List( 1, 2, 3, 4, 5, 6 ), result )
  }

  @Test
  def testAddOneToList() {
    val input = List( 1, 2, 3 )
    val result = List.addOneToList( input )

    Assert.assertEquals( List( 2, 3, 4 ), result )
  }

  @Test
  def testDoubleToString() {
    val input = List( 1.0, 2.0, 3.0 )
    val result = List.doubleToString( input )

    Assert.assertEquals( List( "1.0", "2.0", "3.0" ), result )
  }

  @Test
  def testMap() {
    val input = List( "A", "BB", "CCC" )
    val result = List.map( input )( _.length )

    Assert.assertEquals( List( 1, 2, 3 ), result )
  }

  @Test
  def testFilter() {
    val input = List( 1, 2, 3, 4 )
    val result = List.filter( input )( _ % 2 == 0 )

    Assert.assertEquals( List( 2, 4 ), result )
  }

  @Test
  def testFilterViaFlatMap() {
    val input = List( 1, 2, 3, 4 )
    val result = List.filterViaFlatMap( input )( _ % 2 == 0 )

    Assert.assertEquals( List( 2, 4 ), result )
  }

  @Test
  def testAddListElements() {
    val input1 = List( 1, 2, 3 )
    val input2 = List( 4, 5, 6 )
    val result = List.addListElements( input1, input2 )

    Assert.assertEquals( List( 5, 7, 9 ), result )
  }

  @Test
  def testHasSubsequence() {
    val base = List( 1, 2, 3, 4 )

    Assert.assertTrue( List.hasSubsequence( base, List( 1, 2 ) ) )
    Assert.assertTrue( List.hasSubsequence( base, List( 2, 3 ) ) )
    Assert.assertTrue( List.hasSubsequence( base, List( 4 ) ) )

    Assert.assertTrue( List.hasSubsequence( List( 2, 3, 4 ), List( 3 ) ) )
    Assert.assertTrue( List.hasSubsequence( List( 1, 2, 1, 2, 3, 4 ), List( 1, 2, 3 ) ) )

    Assert.assertFalse( List.hasSubsequence( Nil, List( 1, 3 ) ) )
    Assert.assertFalse( List.hasSubsequence( List( 3, 4 ), List( 1, 3 ) ) )
    Assert.assertFalse( List.hasSubsequence( List( 2, 3, 4 ), List( 1, 3 ) ) )
    Assert.assertFalse( List.hasSubsequence( List( 1, 2, 3, 4 ), List( 1, 3 ) ) )
    Assert.assertFalse( List.hasSubsequence( List( 1, 2, 3 ), List( 1, 2, 3, 4 ) ) )
  }
}
