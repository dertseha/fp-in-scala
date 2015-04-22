package at.sevensuns.dertseha.reading.fp.ch03.datastructures

import org.junit._

@Test
class TreeTest {

  @Test
  def testSizeReturnsNumberOfLeavesAndBranches() = {
    val tree = Branch( Branch( Leaf( 1 ), Leaf( 2 ) ), Leaf( 4 ) )
    val result = Tree.size( tree )

    Assert.assertEquals( 5, result )
  }

  @Test
  def testMaximumReturnsHighestLeaf() = {
    val tree = Branch( Branch( Leaf( 1 ), Branch( Leaf( 90 ), Leaf( 5 ) ) ), Leaf( 4 ) )
    val result = Tree.maximum( tree )

    Assert.assertEquals( 90, result )
  }

  @Test
  def testDepth() = {
    val tree = Branch( Branch( Leaf( 1 ), Branch( Leaf( 90 ), Leaf( 5 ) ) ), Leaf( 4 ) )
    val result = Tree.depth( tree )

    Assert.assertEquals( 4, result )
  }

}
