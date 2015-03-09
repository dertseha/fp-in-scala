package at.sevensuns.dertseha.reading.fp.ch02.gettingstarted

import org.junit._

@Test
class MyModuleTest {

  @Test
  def testFibOf0Is0() = {
    Assert.assertEquals( 0, MyModule.fib( 0 ) )
  }

  @Test
  def testFibOf1Is1() = {
    Assert.assertEquals( 1, MyModule.fib( 1 ) )
  }

  @Test
  def testFibOf6Is8() = {
    Assert.assertEquals( 8, MyModule.fib( 6 ) )
  }

  @Test
  def testIsSortedReturnsTrueForEmptyArray() = {
    Assert.assertTrue( MyModule.isSorted( Array(), ( x: Int, y: Int ) => true ) )
  }

  @Test
  def testIsSortedReturnsTrueForSortedArray() = {
    Assert.assertTrue( MyModule.isSorted( Array( 1, 2, 3, 4 ), ( x: Int, y: Int ) => x < y ) )
  }

  @Test
  def testIsSortedReturnsFalseForMixedArray() = {
    Assert.assertFalse( MyModule.isSorted( Array( 1, 2, 4, 3, 5 ), ( x: Int, y: Int ) => x < y ) )
  }

  @Test
  def testIsSortedWorksWithStrings() = {
    val testArray = Array( "a", "b", "c" )
    Assert.assertTrue( MyModule.isSorted( testArray, ( x: String, y: String ) => x.compareTo( y ) < 0 ) )
  }
}
