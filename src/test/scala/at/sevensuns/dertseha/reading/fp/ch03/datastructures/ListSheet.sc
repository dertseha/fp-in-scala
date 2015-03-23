package at.sevensuns.dertseha.reading.fp.ch03.datastructures

object ListSheet {
	val ex1: List[Double] = Nil               //> ex1  : at.sevensuns.dertseha.reading.fp.ch03.datastructures.List[Double] = N
                                                  //| il
	val ex2: List[Int] = Cons(1, Nil)         //> ex2  : at.sevensuns.dertseha.reading.fp.ch03.datastructures.List[Int] = Cons
                                                  //| (1,Nil)
	val ex3: List[String] = Cons("a", Cons("b", Nil))
                                                  //> ex3  : at.sevensuns.dertseha.reading.fp.ch03.datastructures.List[String] = C
                                                  //| ons(a,Cons(b,Nil))

	// Exercise 3.1
	val x = List(1,2,3,4,5) match {
		case Cons(x, Cons(2, Cons(4, _))) => x
		case Nil => 42
		case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
		case Cons(h, t) => h + List.sum(t)
		case _ => 101
	}                                         //> x  : Int = 3
}