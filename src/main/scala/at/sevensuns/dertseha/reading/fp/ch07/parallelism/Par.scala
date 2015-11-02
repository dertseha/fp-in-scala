package at.sevensuns.dertseha.reading.fp.ch07.parallelism

import java.util.concurrent.TimeUnit
import java.util.concurrent.Future
import java.util.concurrent.ExecutorService
import java.util.concurrent.Callable

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A]( s: ExecutorService )( a: Par[A] ): Future[A] = a( s )

  def unit[A]( a: A ): Par[A] = ( es: ExecutorService ) => UnitFuture( a )

  def lazyUnit[A]( a: => A ): Par[A] = fork( unit( a ) )

  private case class UnitFuture[A]( get: A ) extends Future[A] {
    def isDone = true
    def get( timeout: Long, units: TimeUnit ) = get
    def isCancelled = false
    def cancel( evenIfRunning: Boolean ): Boolean = false
  }

  def map2[A, B, C]( a: Par[A], b: Par[B] )( f: ( A, B ) => C ): Par[C] =
    ( es: ExecutorService ) => {
      val af = a( es )
      val bf = b( es )
      UnitFuture( f( af.get, bf.get ) )
    }

  def fork[A]( a: => Par[A] ): Par[A] =
    es => es.submit( new Callable[A] {
      def call = a( es ).get
    } )

  def delay[A]( fa: => Par[A] ): Par[A] =
    es => fa( es )

  def sortParViaMap2( parList: Par[List[Int]] ): Par[List[Int]] =
    map2( parList, unit( () ) )( ( a, _ ) => a.sorted )

  def map[A, B]( pa: Par[A] )( f: A => B ): Par[B] =
    map2( pa, unit( () ) )( ( a, _ ) => f( a ) )

  def sortPar( parList: Par[List[Int]] ) = map( parList )( _.sorted )

  def parMap[A, B]( ps: List[A] )( f: A => B ): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map( asyncF( f ) )
    sequence( fbs )
  }

  // Exercise 7.4
  def asyncF[A, B]( f: A => B ): A => Par[B] = ???

  // Exercise 7.5 -- this is hard? does this not work or am I already tuned to the basics?
  def sequence[A]( ps: List[Par[A]] ): Par[List[A]] =
    ps.foldRight( unit( Nil: List[A] ) )( ( elem, parAcc ) => map2( elem, parAcc )( ( single, acc ) => single :: acc ) )

  // Exercise 7.6
  def parFilter[A]( as: List[A] )( f: A => Boolean ): Par[List[A]] = ???
  // Where to put the synchronous filter? otherwise we'd need a flatMap

  // Exercise 7.11
  def choiceN[A]( n: Par[Int] )( choices: List[Par[A]] ): Par[A] =
    map2( n, sequence( choices ) )( ( index, list ) => list.drop( index ).head )

  def choice[A]( cond: Par[Boolean] )( t: Par[A], f: Par[A] ): Par[A] =
    choiceN( map( cond )( result => if ( result ) 1 else 0 ) )( t :: f :: Nil )

  // Exercise 7.13
  def chooser[A, B]( pa: Par[A] )( choices: A => Par[B] ): Par[B] = ??? // need flatMap!
  //map2( pa, unit( choices ) )( ( index, getter ) => getter( index ) )

  // Exercise 7.14
  def flatMap[A, B]( a: Par[A] )( f: A => Par[B] ): Par[B] =
    ( es: ExecutorService ) => {
      val af = a( es )
      f( af.get )( es )
    }

  def join[A]( a: Par[Par[A]] ): Par[A] =
    ( es: ExecutorService ) => {
      val af = a( es )
      af.get()( es )
    }

  def flatMapViaJoin[A, B]( a: Par[A] )( f: A => Par[B] ): Par[B] =
    join( map( a )( f ) )

  def joinViaFlatMap[A]( a: Par[Par[A]] ): Par[A] =
    flatMap( a )( identity )
}
