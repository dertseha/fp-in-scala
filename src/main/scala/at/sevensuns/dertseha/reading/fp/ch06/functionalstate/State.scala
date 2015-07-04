package at.sevensuns.dertseha.reading.fp.ch06.functionalstate

case class State[S, +A]( run: S => ( A, S ) ) {
  // Exercise 6.10a
  def flatMap[B]( f: A => State[S, B] ): State[S, B] =
    State( state => {
      val ( a, state2 ) = run( state )
      f( a ).run( state2 )
    } )

  def map[B]( f: A => B ): State[S, B] =
    flatMap( a => State( state => ( f( a ), state ) ) )

  def modify[S]( f: S => S ): State[S, Unit] = for {
    s <- get
    _ <- set( f( s ) )
  } yield ()

  def get[S]: State[S, S] = State( s => ( s, s ) )

  def set[S]( s: S ): State[S, Unit] = State( _ => ( (), s ) )
}

object State {

  type Rand[+A] = State[RNG, A]

  // Exercise 6.10b
  def unit[S, A]( a: A ): State[S, A] = State( s => ( a, s ) )

  def map2WithFlatMap[S, A, B, C]( ra: State[S, A], rb: State[S, B] )( f: ( A, B ) => C ): State[S, C] =
    ra.flatMap( a => rb.flatMap( b => State( state => ( f( a, b ), state ) ) ) )

  def sequence[S, A]( fs: List[State[S, A]] ): State[S, List[A]] =
    State( state => fs.foldLeft( ( Nil: List[A], state ) )( ( acc, elem ) => {
      val ( result, rng2 ) = elem.run( acc._2 )
      ( result :: acc._1, rng2 )
    } ) )

  // Examples
  val int: Rand[Int] = State( _.nextInt )

  def ints( count: Int ): Rand[List[Int]] =
    sequence( List.fill( count )( int ) )

  val nsWeird: Rand[List[Int]] =
    int.flatMap( x =>
      int.flatMap( y =>
        ints( x ).map( xs =>
          xs.map( _ % y ) ) ) )

  val nsComprehensible: Rand[List[Int]] = for {
    x <- int
    y <- int
    xs <- ints( x )
  } yield xs.map( _ % y )
}
