package at.sevensuns.dertseha.reading.fp.ch11.monads

object IdWorksheet {
  Id( "Hello, " ) flatMap ( a => Id( "monad!" ) flatMap ( b => Id( a + b ) ) )
                                                  //> res0: at.sevensuns.dertseha.reading.fp.ch11.monads.Id[String] = Id(Hello, mo
                                                  //| nad!)

  for {
    a <- Id( "Hello, " )
    b <- Id( "monad!" )
  } yield a + b                                   //> res1: at.sevensuns.dertseha.reading.fp.ch11.monads.Id[String] = Id(Hello, mo
                                                  //| nad!)
}