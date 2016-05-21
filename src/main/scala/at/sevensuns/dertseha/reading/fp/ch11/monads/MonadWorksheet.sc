package at.sevensuns.dertseha.reading.fp.ch11.monads
import at.sevensuns.dertseha.reading.fp.ch04.handlingerrors.Option
import at.sevensuns.dertseha.reading.fp.ch04.handlingerrors.Some
import at.sevensuns.dertseha.reading.fp.ch04.handlingerrors.None

object MonadWorksheet {
	val aList = List('a', 'b', 'c')           //> aList  : List[Char] = List(a, b, c)
	val listM = Monad.listMonad               //> listM  : at.sevensuns.dertseha.reading.fp.ch11.monads.Monad[List] = at.seven
                                                  //| suns.dertseha.reading.fp.ch11.monads.Monad$$anon$3@3b81a1bc
	val listReplicate = listM.replicateM(2, aList)
                                                  //> listReplicate  : List[List[Char]] = List(List(a, a), List(a, b), List(a, c),
                                                  //|  List(b, a), List(b, b), List(b, c), List(c, a), List(c, b), List(c, c))
	
	
	
	
	
	
	
 val anOption = None: Option[Int]                 //> anOption  : at.sevensuns.dertseha.reading.fp.ch04.handlingerrors.Option[Int]
                                                  //|  = None
 val optM = Monad.optionMonad                     //> optM  : at.sevensuns.dertseha.reading.fp.ch11.monads.Monad[at.sevensuns.dert
                                                  //| seha.reading.fp.ch04.handlingerrors.Option] = at.sevensuns.dertseha.reading.
                                                  //| fp.ch11.monads.Monad$$anon$2@2ef9b8bc
 
 val optReplicate = optM.replicateM(3, anOption)  //> optReplicate  : at.sevensuns.dertseha.reading.fp.ch04.handlingerrors.Option[
                                                  //| List[Int]] = None
 

 val anOption5 = Some(5): Option[Int]             //> anOption5  : at.sevensuns.dertseha.reading.fp.ch04.handlingerrors.Option[Int
                                                  //| ] = Some(5)
 
 val optReplicate5 = optM.replicateM(3, anOption5)//> optReplicate5  : at.sevensuns.dertseha.reading.fp.ch04.handlingerrors.Option
                                                  //| [List[Int]] = Some(List(5, 5, 5))
 val optReplicateOur5 = optM.replicateOur(3, anOption5)
                                                  //> optReplicateOur5  : at.sevensuns.dertseha.reading.fp.ch04.handlingerrors.Opt
                                                  //| ion[List[Int]] = Some(List(5, 5, 5))


	////////////////////////////////
	
	val ourNiceFunction = (a: Int) =>
		List(a % 2 == 0, a % 3 == 0)      //> ourNiceFunction  : Int => List[Boolean] = <function1>

	ourNiceFunction(1)                        //> res0: List[Boolean] = List(false, false)
	ourNiceFunction(3)                        //> res1: List[Boolean] = List(false, true)
	ourNiceFunction(6)                        //> res2: List[Boolean] = List(true, true)
	ourNiceFunction(8)                        //> res3: List[Boolean] = List(true, false)

	Monad.listMonad.filterM(List(1, 3, 6, 7, 8))(ourNiceFunction)
                                                  //> res4: List[List[Int]] = List(List(6, 8), List(6), List(6, 8), List(6), List(
                                                  //| 6, 8), List(6), List(6, 8), List(6), List(3, 6, 8), List(3, 6), List(3, 6, 8
                                                  //| ), List(3, 6), List(3, 6, 8), List(3, 6), List(3, 6, 8), List(3, 6), List(6,
                                                  //|  8), List(6), List(6, 8), List(6), List(6, 8), List(6), List(6, 8), List(6),
                                                  //|  List(3, 6, 8), List(3, 6), List(3, 6, 8), List(3, 6), List(3, 6, 8), List(3
                                                  //| , 6), List(3, 6, 8), List(3, 6))

	Monad.listMonad.filterMOfficial(List(1, 3, 6, 7, 8))(ourNiceFunction)
                                                  //> res5: List[List[Int]] = List(List(6, 8), List(6), List(6, 8), List(6), List(
                                                  //| 6, 8), List(6), List(6, 8), List(6), List(3, 6, 8), List(3, 6), List(3, 6, 8
                                                  //| ), List(3, 6), List(3, 6, 8), List(3, 6), List(3, 6, 8), List(3, 6), List(6,
                                                  //|  8), List(6), List(6, 8), List(6), List(6, 8), List(6), List(6, 8), List(6),
                                                  //|  List(3, 6, 8), List(3, 6), List(3, 6, 8), List(3, 6), List(3, 6, 8), List(3
                                                  //| , 6), List(3, 6, 8), List(3, 6))
	
		Monad.listMonad.filterMMy(List(1, 3, 6, 7, 8))(ourNiceFunction)
                                                  //> res6: List[List[Int]] = List(List(6, 8), List(6), List(6, 8), List(6), List
                                                  //| (6, 8), List(6), List(6, 8), List(6), List(3, 6, 8), List(3, 6), List(3, 6,
                                                  //|  8), List(3, 6), List(3, 6, 8), List(3, 6), List(3, 6, 8), List(3, 6), List
                                                  //| (6, 8), List(6), List(6, 8), List(6), List(6, 8), List(6), List(6, 8), List
                                                  //| (6), List(3, 6, 8), List(3, 6), List(3, 6, 8), List(3, 6), List(3, 6, 8), L
                                                  //| ist(3, 6), List(3, 6, 8), List(3, 6))

	Monad.listMonad.filterM(List(1))(ourNiceFunction)
                                                  //> res7: List[List[Int]] = List(List(), List())
	
	val ourNiceOptionFunction = (a: Int) =>
		Some(a % 2 == 0)                  //> ourNiceOptionFunction  : Int => at.sevensuns.dertseha.reading.fp.ch04.handl
                                                  //| ingerrors.Some[Boolean] = <function1>
	Monad.optionMonad.filterM(List(1, 3, 6, 7, 8))(ourNiceOptionFunction)
                                                  //> res8: at.sevensuns.dertseha.reading.fp.ch04.handlingerrors.Option[List[Int]
                                                  //| ] = Some(List(6, 8))
	
}