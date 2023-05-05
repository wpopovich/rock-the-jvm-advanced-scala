package lectures.part1as

object DarkSugars extends App {
  // Syntax 1: Methods with single params
  def singleArgumentMethod(arg: Int) = s"$arg little ducks"
  val desc = singleArgumentMethod {
    //stuff is happening here
    42
  }

  println(desc)

  //Syntax 2: Single abstract method
  trait Action {
    def act(x: Int) : Int
  }

  val action: Action = (x: Int) => x + 1

  class MyAccumulator(value: Int) {
    def -->:(n: Int) : MyAccumulator = new MyAccumulator(value + n)
    def -->(n: Int) : MyAccumulator = new MyAccumulator(value + n)
    def print = println(value)
  }

  (2 -->: 3 -->: new MyAccumulator(0)).print
  (new MyAccumulator(0) --> 2 --> 3 --> 4).print

  val x = Array(1,2,3)

  //Syntax Sugar #7: setters for mutable
  class Mutable {
    private var internalValue : Int = 0
    def internal = internalValue
    def internal_=(x : Int): Unit = internalValue = x
  }

  val mutable = new Mutable
  mutable.internal = 300
  println(mutable.internal)
}
