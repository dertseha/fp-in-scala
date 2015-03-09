package at.sevensuns.dertseha.reading.fp.ch02.gettingstarted

object MyModuleSheet {

	MyModule.abs(-10)                         //> res0: Int = 10

	MyModule.formatResult("absolute value", -42, MyModule.abs)
                                                  //> res1: String = The absolute value of -42 is 42.
	MyModule.formatResult("factorial", 4, MyModule.factorial)
                                                  //> res2: String = The factorial of 4 is 24.
	MyModule.formatResult("fibonacci", 10, MyModule.fib)
                                                  //> res3: String = The fibonacci of 10 is 55.

	MyModule.findFirst(Array(7, 9, 13), (x: Int) => x == 9)
                                                  //> res4: Int = 1
}