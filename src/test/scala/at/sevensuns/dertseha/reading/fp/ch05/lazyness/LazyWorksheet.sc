package at.sevensuns.dertseha.reading.fp.ch05.lazyness

object LazyWorksheet {
  Stream.ones.take(5).toList                      //> res0: List[Int] = List(1, 1, 1, 1, 1)
  //Stream.ones.map(_ + 1).exists(_ % 2 == 0)
 	Stream.ones.takeWhile(_ == 1)             //> res1: at.sevensuns.dertseha.reading.fp.ch05.lazyness.Stream[Int] = Cons(<fun
                                                  //| ction0>,<function0>)
 	Stream.ones.forAll(_ != 1)                //> res2: Boolean = false
}