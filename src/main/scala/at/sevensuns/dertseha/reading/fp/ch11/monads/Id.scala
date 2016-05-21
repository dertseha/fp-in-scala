package at.sevensuns.dertseha.reading.fp.ch11.monads

case class Id[A]( value: A ) {
  def map[B]( f: A => B ) = Id( f( value ) )

  def flatMap[B]( f: A => Id[B] ) = f( value )
}

