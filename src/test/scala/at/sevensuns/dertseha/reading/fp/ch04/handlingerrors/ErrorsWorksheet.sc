package at.sevensuns.dertseha.reading.fp.ch04.handlingerrors

object ErrorsWorksheet {
  def failingFn( i: Int ): Int = {
    val y: Int = throw new Exception( "fail!" )
    try {
      val x = 42 + 5
      x + y
    } catch { case e: Exception => 43 }
  }                                               //> failingFn: (i: Int)Int
  
  failingFn(12)                                   //> java.lang.Exception: fail!
                                                  //| 	at at.sevensuns.dertseha.reading.fp.ch04.handlingerrors.ErrorsWorksheet$
                                                  //| $anonfun$main$1.failingFn$1(at.sevensuns.dertseha.reading.fp.ch04.handlinger
                                                  //| rors.ErrorsWorksheet.scala:5)
                                                  //| 	at at.sevensuns.dertseha.reading.fp.ch04.handlingerrors.ErrorsWorksheet$
                                                  //| $anonfun$main$1.apply$mcV$sp(at.sevensuns.dertseha.reading.fp.ch04.handlinge
                                                  //| rrors.ErrorsWorksheet.scala:12)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at at.sevensuns.dertseha.reading.fp.ch04.handlingerrors.ErrorsWorksheet$
                                                  //| .main(at.sevensuns.dertseha.reading.fp.ch04.handlingerrors.ErrorsWorksheet.s
                                                  //| cala:3)
                                                  //| 	at at.sevensuns.dertseha.reading.fp.ch04.handlingerrors
                                                  //| Output exceeds cutoff limit.

}