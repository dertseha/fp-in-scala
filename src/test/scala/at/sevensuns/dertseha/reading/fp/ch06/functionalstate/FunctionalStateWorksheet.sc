package at.sevensuns.dertseha.reading.fp.ch06.functionalstate

object FunctionalStateWorksheet {
  val rng = SimpleRNG(42)                         //> rng  : at.sevensuns.dertseha.reading.fp.ch06.functionalstate.SimpleRNG = Sim
                                                  //| pleRNG(42)
  val (n1, rng2) = rng.nextInt                    //> n1  : Int = 16159453
                                                  //| rng2  : at.sevensuns.dertseha.reading.fp.ch06.functionalstate.RNG = SimpleRN
                                                  //| G(1059025964525)
  val (n2, rng3) = rng2.nextInt                   //> n2  : Int = -1281479697
                                                  //| rng3  : at.sevensuns.dertseha.reading.fp.ch06.functionalstate.RNG = SimpleRN
                                                  //| G(197491923327988)
                                                  
	RNG.int(rng)                              //> res0: (Int, at.sevensuns.dertseha.reading.fp.ch06.functionalstate.RNG) = (16
                                                  //| 159453,SimpleRNG(1059025964525))
}