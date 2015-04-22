package at.sevensuns.dertseha.reading.fp.ch03.datastructures

sealed trait Tree[+A]
case class Leaf[A]( value: A ) extends Tree[A]
case class Branch[A]( left: Tree[A], right: Tree[A] ) extends Tree[A]

object Tree {

  // This 'fold' I came up with after reading attempting the second exercise (3.26).
  // I saw similarity with List.fold() and created it, though I thought I need an init value.
  def fold[A, B]( tree: Tree[A], init: B )( f: ( A, B ) => B )( joiner: ( B, B ) => B ): B = tree match {
    case Leaf( a )             => f( a, init )
    case Branch( left, right ) => joiner( fold( left, init )( f )( joiner ), fold( right, init )( f )( joiner ) )
  }

  // Exercise 3.25
  def size[A]( tree: Tree[A] ): Int =
    fold( tree, 0 )( ( a, acc ) => acc + 1 )( _ + _ + 1 )

  // Exercise 3.26
  def maximum( tree: Tree[Int] ): Int = {
    val maxer = ( x: Int, y: Int ) => x max y

    fold( tree, 0 )( maxer )( maxer )
  }

  // Exercise 3.27
  def depth( tree: Tree[Int] ): Int =
    fold( tree, 0 )( ( a, acc ) => 1 max acc )( ( a, b ) => ( a max b ) + 1 )

  // Exercise 3.28
  def map[A, B]( tree: Tree[A] )( f: A => B ): Tree[B] =
    tree match {
      case Leaf( a )             => Leaf( f( a ) )
      case Branch( left, right ) => Branch( map( left )( f ), map( right )( f ) )
    }

  def fold[A, B]( tree: Tree[A] )( f: A => B )( j: ( B, B ) => B ): B = tree match {
    case Leaf( a )             => f( a )
    case Branch( left, right ) => j( fold( left )( f )( j ), fold( right )( f )( j ) )
  }

  def newSize[A]( tree: Tree[A] ): Int =
    fold( tree )( _ => 1 )( _ + _ + 1 )

  def newMaximum( tree: Tree[Int] ): Int =
    fold( tree )( identity )( ( a, b ) => a max b )

  def newDepth( tree: Tree[Int] ): Int =
    fold( tree )( _ => 1 )( ( a, b ) => ( a max b ) + 1 )

  def newMap[A, B]( tree: Tree[A] )( f: A => B ): Tree[B] =
    fold( tree )( a => Leaf( f( a ) ): Tree[B] )( ( left, right ) => Branch( left, right ) )
}
