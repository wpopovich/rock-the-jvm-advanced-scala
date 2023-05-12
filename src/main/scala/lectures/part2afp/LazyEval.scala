package lectures.part2afp

object LazyEval extends App {

  lazy val x : Int = {
    println("hello")
    42
  }

  println(x)
  println(x)

  //examples
  //With side effects
  def sideEffectCondition : Boolean = {
    println("Boo")
    true
  }
  def simpleCondition : Boolean ={
    false
  }

  lazy val lazyCondition = sideEffectCondition
  println(if (simpleCondition && lazyCondition) "yes" else "no")

  //In conjunction with call by name
  def byNameMethod(n : => Int) = {
    //CALL BY NEED
    lazy val t = n // Without this, we evaluate n everytime its called
    t + t + t + 1
  }
  def retrieveMagicValue = {
    Thread.sleep(1000)
    println("Waiting")
    42
  }

  println(byNameMethod(retrieveMagicValue))

  // Filtering with lazy val
  def lessThan30(n: Int) : Boolean = {
    println(s"$n less than 30")
    n < 30
  }

  def greaterThan20(n: Int): Boolean = {
    println(s"$n greater than 20")
    n > 20
  }

  val numbers = List(1,25,40,5, 23)
//  val lt30 = numbers.filter(lessThan30)
//  val gt20 = numbers.filter(greaterThan20)
  val both  = numbers.filter(lessThan30).filter(greaterThan20)

//  println(lt30)
//  println(gt20)
  println(both)

  val lt30lazy = numbers.withFilter(lessThan30)
  val lt20lazy = lt30lazy.withFilter(greaterThan20)

  //For-Comprehension use withFilter with guards
  for {
    a <- List(1,2,3) if (a % 2 == 0)
  } yield a + 1
  List(1,2,3).withFilter(_ % 2 ==0).map(_ + 1)

  /*
  Exercise
  implement a lazyly evaluated, single linked stream of elements
  Head of the stream is always available, but the tail is lazy

  naturals = MyStream.from(1)(x => x + 1) = stream of natural numbers (potentially infinite)
  naturals.take(100) = lazily evaluated stream of the first 100 naturals
  naturals.map(_ * 2) = stream of all even numbers
   */
  abstract class MyStream[+A] {
    def isEmpty : Boolean
    def head: A
    def tail: MyStream[A]
    def #::[B >: A](elem: B) : MyStream[B] // Prepend operator
    def ++[B >: A](anotherStream: MyStream[B]) : MyStream[B] // concatenate 2 sterams
    def foreach(f: A => Unit) : Unit
    def map[B](f: A => B) : MyStream[B]
    def flatMap[B](f: A => MyStream[B]) : MyStream[B]
    def filter(predicate: A => Boolean) : MyStream[A]
    def take(n: Int) : MyStream[A] //takes the first n elements out of the stream
    def takeAsList(n: Int) : List[A]
  }

  case class EmptyStreamImpl[A]() extends MyStream {
    override def isEmpty: Boolean = true

    override def head: Nothing = ???

    override def tail: MyStream[Nothing] = ???

    override def #::[B >: A](elem: B): MyStream[B] = ???

    override def ++[B >: Nothing](anotherStream: MyStream[B]): MyStream[B] = ???

    override def foreach(f: Nothing => Unit): Unit = ???

    override def map[B](f: Nothing => B): MyStream[B] = ???

    override def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = ???

    override def filter(predicate: Nothing => Boolean): MyStream[Nothing] = ???

    override def take(n: Int): MyStream[Nothing] = ???

    override def takeAsList(n: Int): List[Nothing] = ???
  }

  class MyStreamImpl[A](val head: A)(generator: A => A) extends MyStream[A] {
    override def isEmpty: Boolean = false
    override def tail: MyStream[A] = new MyStreamImpl[A](generator(head))(generator)
    override def #::[B >: A](elem: B): MyStream[B] = ???
    override def ++[B >: A](anotherStream: MyStream[B]): MyStream[B] = ???
    override def foreach(f: A => Unit): Unit = {
      f(head)
      tail.foreach(f)
    }

    override def map[B](f: A => B): MyStream[B] = ???
    override def flatMap[B](f: A => MyStream[B]): MyStream[B] = ???
    override def filter(predicate: A => Boolean): MyStream[A] = ???
    override def take(n: Int): MyStream[A] = {
      def newGenerator(limit: Int, currentCount: Int, generator: A => A): Unit = {
      
}
      if (count > n) this
      else new MyStreamImpl[A](generator(head))
    }

    override def takeAsList(n: Int): List[A] = ???
  }

  object MyStream {
    def from[A](start: A)(generator: A => A) : MyStream[A] = new MyStreamImpl[A](start)(generator)
  }
  val stream = MyStream.from(1)(_ + 1)
  println(stream)
  stream.take(100).foreach(println)
  //Tests

}
