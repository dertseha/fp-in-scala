package at.sevensuns.dertseha.reading.fp.ch07.parallelism

import java.util.concurrent.TimeUnit
import java.util.concurrent.Future
import java.util.concurrent.ExecutorService
import java.util.concurrent.Callable

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A]( s: ExecutorService )( a: Par[A] ): Future[A] = a( s )

  def unit[A]( a: A ): Par[A] = ( es: ExecutorService ) => UnitFuture( a )

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

  // Exercise 7.5
  def sequence[A]( ps: List[Par[A]] ): Par[List[A]] = ???

}
