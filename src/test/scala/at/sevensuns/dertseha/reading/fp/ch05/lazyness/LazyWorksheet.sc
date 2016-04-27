package at.sevensuns.dertseha.reading.fp.ch05.lazyness

object LazyWorksheet {
  Stream.ones.take(5).toList                      //> res0: List[Int] = List(1, 1, 1, 1, 1)
  //Stream.ones.map(_ + 1).exists(_ % 2 == 0)
 	Stream.ones.takeWhile(_ == 1)             //> res1: at.sevensuns.dertseha.reading.fp.ch05.lazyness.Stream[Int] = Cons(<fun
                                                  //| ction0>,<function0>)
 	Stream.ones.forAll(_ != 1)                //> res2: Boolean = false
 	
 	
 	def maybeTwice(b: Boolean, i: => Int) = if (b) i+i else 0
                                                  //> maybeTwice: (b: Boolean, i: => Int)Int
 	val x = maybeTwice(true, { println("hi"); 1+41 })
                                                  //> hi
                                                  //| hi
                                                  //| x  : Int = 84
 	def maybeTwice2(b: Boolean, i: => Int) = {
	 	lazy val j = i
	 	if (b) j+j else 0
 	}                                         //> maybeTwice2: (b: Boolean, i: => Int)Int
 
 	val x2 = maybeTwice2(true, { println("hi"); 1+41 })
                                                  //> hi
                                                  //| x2  : Int = 84

 	def thingie () = { println("hi"); 1+41 }  //> thingie: ()Int
 	val x30 = maybeTwice2(true, thingie)      //> hi
                                                  //| x30  : Int = 84
 	val x31 = maybeTwice2(true, thingie)      //> hi
                                                  //| x31  : Int = 84

 	def maybeTwice3(b: Boolean, i: => Int) = {
	 	if (b) {
			val j = i
	 		j+j
	 	} else 0
 	}                                         //> maybeTwice3: (b: Boolean, i: => Int)Int

	maybeTwice3(true, { println("hi3"); 1+30 })
                                                  //> hi3
                                                  //| res3: Int = 62

	def identity[A](a: A): A = a              //> identity: [A](a: A)A
}