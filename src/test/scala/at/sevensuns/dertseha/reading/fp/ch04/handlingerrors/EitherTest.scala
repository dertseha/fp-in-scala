package at.sevensuns.dertseha.reading.fp.ch04.handlingerrors

import org.junit._

@Test
class EitherTest {

  @Test
  def testSequenceReturnsEmptyListOnEmpty() = {
    val result = Either.sequence( List() )

    Assert.assertEquals( Right( List() ), result )
  }

  @Test
  def testSequenceReturnsLeftOnListOfOneLeft() = {
    val result = Either.sequence( List( Left( "test" ) ) )

    Assert.assertEquals( Left( "test" ), result )
  }

  @Test
  def testSequenceReturnsLeftOnListWithALeft() {
    val result = Either.sequence( List( Right( 1234 ), Left( "other" ) ) )

    Assert.assertEquals( Left( "other" ), result )
  }

  @Test
  def testSequenceReturnsRightOnListWithOnlyRights() {
    val result = Either.sequence( List( Right( 1234 ), Right( 5678 ) ) )

    Assert.assertEquals( Right( List( 1234, 5678 ) ), result )
  }
}
