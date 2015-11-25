package at.sevensuns.dertseha.reading.fp.ch08.testing

trait Prop {
  def checkBool: Boolean
  def check: Either[( Prop.FailedCase, Prop.SuccessCount ), Prop.SuccessCount]

  def &&( p: Prop ): Prop = new AndProp( this, p )
}

class AndProp( a: Prop, b: Prop ) extends Prop {
  def checkBool: Boolean = a.checkBool && b.checkBool
  def check: Either[( Prop.FailedCase, Prop.SuccessCount ), Prop.SuccessCount] = ???
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}
