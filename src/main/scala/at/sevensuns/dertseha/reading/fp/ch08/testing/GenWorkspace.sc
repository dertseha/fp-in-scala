package at.sevensuns.dertseha.reading.fp.ch08.testing

import at.sevensuns.dertseha.reading.fp.ch06.functionalstate.SimpleRNG

object GenWorkspace {
  //println("Welcome to the Scala worksheet")
  
  val x = Gen.choose(0, 100)                      //> x  : at.sevensuns.dertseha.reading.fp.ch08.testing.Gen[Int] = Gen(State(<fun
                                                  //| ction1>))
                  
  val rng0 = new SimpleRNG(42)                    //> rng0  : at.sevensuns.dertseha.reading.fp.ch06.functionalstate.SimpleRNG = Si
                                                  //| mpleRNG(42)

  val (result1, rng1) = x.sample.run(rng0)        //> result1  : Int = 53
                                                  //| rng1  : at.sevensuns.dertseha.reading.fp.ch06.functionalstate.RNG = SimpleRN
                                                  //| G(1059025964525)
  val (result2, rng2) = x.sample.run(rng1)        //> result2  : Int = 97
                                                  //| rng2  : at.sevensuns.dertseha.reading.fp.ch06.functionalstate.RNG = SimpleRN
                                                  //| G(197491923327988)
  val (result3, rng3) = x.sample.run(rng2)        //> result3  : Int = 2
                                                  //| rng3  : at.sevensuns.dertseha.reading.fp.ch06.functionalstate.RNG = SimpleRN
                                                  //| G(259172689157871)

	val intGen = Gen.choose(1, 6)             //> intGen  : at.sevensuns.dertseha.reading.fp.ch08.testing.Gen[Int] = Gen(State
                                                  //| (<function1>))
val listGen =	intGen.flatMap ( x => Gen.listOfNManual(x, Gen.choose(1,2)) )
                                                  //> listGen  : at.sevensuns.dertseha.reading.fp.ch08.testing.Gen[List[Int]] = Ge
                                                  //| n(State(<function1>))
                                                  
val resultList = listGen.sample.run(rng0)         //> resultList  : (List[Int], at.sevensuns.dertseha.reading.fp.ch06.functionalst
                                                  //| ate.RNG) = (List(1, 1, 1, 1),SimpleRNG(115998806404289))
 val elementGen = Gen.choose(10, 21)              //> elementGen  : at.sevensuns.dertseha.reading.fp.ch08.testing.Gen[Int] = Gen(S
                                                  //| tate(<function1>))

val newListGen = elementGen.listOfN(intGen)       //> newListGen  : at.sevensuns.dertseha.reading.fp.ch08.testing.Gen[List[Int]] =
                                                  //|  Gen(State(<function1>))
val crazy = newListGen.sample.run(rng1)           //> crazy  : (List[Int], at.sevensuns.dertseha.reading.fp.ch06.functionalstate.R
                                                  //| NG) = (List(18, 13, 12),SimpleRNG(115998806404289))

}