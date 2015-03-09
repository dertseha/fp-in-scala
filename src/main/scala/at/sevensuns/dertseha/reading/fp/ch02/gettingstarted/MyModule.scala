package at.sevensuns.dertseha.reading.fp.ch02.gettingstarted

// A comment!
/* Another comment */
/** A documentation comment */
object MyModule {

  def main( args: Array[String] ): Unit = {
    println( formatAbs( -42 ) )
    println( formatFactorial( 7 ) )
  }

  private def formatAbs( x: Int ) = {
    val msg = "The absolute value of %d is %d"
    msg.format( x, abs( x ) )
  }

  private def formatFactorial( n: Int ) = {
    val msg = "The factorial of %d is %d."
    msg.format( n, factorial( n ) )
  }

  def formatResult( name: String, n: Int, f: Int => Int ) = {
    val msg = "The %s of %d is %d."
    msg.format( name, n, f( n ) )
  }

  def abs( n: Int ): Int =
    if ( n < 0 ) -n
    else n

  def factorial( n: Int ): Int = {
    @annotation.tailrec
    def go( n: Int, acc: Int ): Int =
      if ( n <= 0 ) acc
      else go( n - 1, n * acc )
    go( n, 1 )
  }

  def findFirst[A]( as: Array[A], p: A => Boolean ): Int = {
    @annotation.tailrec
    def loop( n: Int ): Int =
      if ( n >= as.length ) -1
      else if ( p( as( n ) ) ) n
      else loop( n + 1 )

    loop( 0 )
  }

  // Exercise 2.1
  def fib( n: Int ): Int = {
    @annotation.tailrec
    def go( remaining: Int, next: Int, sum: Int ): Int =
      if ( remaining > 0 ) go( remaining - 1, next + sum, next ) else sum

    go( n, 1, 0 )
  }

  // Exercise 2.2
  def isSorted[A]( as: Array[A], ordered: ( A, A ) => Boolean ): Boolean = {
    @annotation.tailrec
    def loop( n: Int ): Boolean =
      if ( n >= as.length - 1 ) true
      else if ( ordered( as( n ), as( n + 1 ) ) ) loop( n + 1 )
      else false

    loop( 0 )
  }

  def partial1[A, B, C]( a: A, f: ( A, B ) => C ): B => C =
    b => f( a, b )

  // Exercise 2.3
  def curry[A, B, C]( f: ( A, B ) => C ): A => ( B => C ) =
    a => b => f( a, b )

  // Exercise 2.4
  def uncurry[A, B, C]( f: A => B => C ): ( A, B ) => C =
    ( a, b ) => f( a )( b )

  // Exercise 2.5
  def compose[A, B, C]( f: B => C, g: A => B ): A => C =
    a => f( g( a ) )
}
