package lectures.part2afp

object CurriesPAF extends App {

  //Curried functions
  val curriedFunction: Int => Int => Int => Int = x => y => z => x + y * z

  /*
  Exercises

   */
  val simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int) = x+y
  def curriedAddMethod(x: Int)(y: Int) = x + y
  //add7: Int => Int = y => 7 + y
  //As many different implementations of add7 using the above
  val add7: Int => Int = (x: Int) => simpleAddFunction(7, x)
  val add7_ = curriedAddMethod(7) _ //Partially applied function
  val add7_2 = curriedAddMethod(7)(_) //Partially Applied function
  val add7Again = (x: Int => Int) => x(7)
  val add7Simple: Int => Int = simpleAddMethod(7,_) //Turning methods into function value
  val add7Simple2 : Int => Int = simpleAddFunction.curried(7)

  println(add7(200))
  println(add7_(200))
  println(add7_2(200))
  println(add7Simple(200))
  println(add7Simple2(200))
  println(add7Again(curriedAddMethod(200)))

  //Underscores
  def concatenator(str1: String, str2: String, str3: String) =
    str1 + str2 + str3

  val insertName = concatenator("Hello, I'm ", _, ", how are you?")
  println(insertName("Walter"))

  val fillInTheBlanks = concatenator("Hello, ", _, _)
  println(fillInTheBlanks("Carlos", "Jijijji"))

  /*
    Exercise
    Process a list of numbers and return their string representations
    with different formats
    Using %4.2f, %8.6f and %14.12 with curriend formater function
   */

  val numberFormatter = (format: String, number: Double) =>
    format.format(number)

  def numberFormatter2(s: String)(n:Double) = s.format(n)

//  val _42f = numberFormatter("%4.2f", _)
  val _42f = numberFormatter2("%4.2f") _ //Lifting
  val _86f = numberFormatter("%8.6f", _)
  val _1412f = numberFormatter("%14.12f", _)

  val aSeq = Seq(1,2,Math.PI)
  private def formatList(seq: Seq[Double]) =
    seq.flatMap(e => Seq(_42f(e), _1412f(e), _86f(e)))

  println(formatList(aSeq))

  /*
  Exercise 2
  difference between
    - functions vs methods
    - parameters by name vs 0-lambda

    define 2 small methods byName(n: => Int) = n + 1
    def byFunction(f: () => Int) = f + 1

    def method: Int = 42
    def parenMethod() : Int = 42

    calling byName and byFunction with these expressions
      -int
      -method()
      -parentMethod
      -lambda
      -partial function
      Figure out what compiles and why
   */

  def byName(n: => Int) = n + 1
  def byFunction(f: () => Int) = f() + 1

  def method = 42
  def parenMethod() = 42

  println(byName(5))
  println(byName(method))
  println(byName(parenMethod()))
  println(byName {6})
  println(byFunction(() => 6))
  println(byFunction(method _))
  println(byFunction(parenMethod))
//  println(byFunction(6))

  def whileLoop(condition: => Boolean)(body: => Unit): Unit = {
    if (condition) {
      body
      whileLoop(condition)(body)
    }
  }

  var i = 1
  println("whileLoop")
  whileLoop(i > 2) {
    println(i)
    i -= 1
  }


}
