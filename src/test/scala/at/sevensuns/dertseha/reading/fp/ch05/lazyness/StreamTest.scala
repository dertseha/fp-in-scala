package at.sevensuns.dertseha.reading.fp.ch05.lazyness

import org.junit._

@Test
class StreamTest {

  @Test
  @Ignore
  def testToListConvertsToList() = {
    val input = Stream( 1, 2, 3 )
    val result = input.toList

    Assert.assertEquals( List( 1, 2, 3 ), result )
  }

}
