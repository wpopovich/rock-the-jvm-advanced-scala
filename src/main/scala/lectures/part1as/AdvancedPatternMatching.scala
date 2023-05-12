package lectures.part1as

import lectures.part1as.AdvancedPatternMatching.MyList

import scala.None

object AdvancedPatternMatching extends App {


  class Person(val name: String, val age: Int)

  object Person {
    def unapply(person: Person) : Option[(String, Int)] = {
      Some(person.name, person.age)
    }
  }

  val bob = new Person("Bob", 20)
  val mark = new Person("Mark", 30)


  val personMatch = (person: Person) => person match {
    case Person("Bob", _) => s"Bob is the best"
    case Person(n, _) => s"Hi $n"
  }
  val message = personMatch(bob)

  val message2 = personMatch(mark)
  println(message)
  println(message2)

  //Excercise. Make the following code
  //better with pattern matching
  val n = 20
  val mathProperty = n match {
    case x if x < 10 => "single digit"
    case x if x % 2 == 0 => "even number"
    case _ => "no property"
  }

  //Solution
  object MathProperty {
    object singleDigit {
      def unapply(n: Int) = n < 10
    }

    object even {
      def unapply(n: Int) = n % 2 == 0
    }
  }

  val betterMathProperty = n match {
    case MathProperty.singleDigit() => "single digit"
    case MathProperty.even() => "even number"
    case _ => "No property"
  }

  println(betterMathProperty)

  //infix patter
  case class Or[A,B](a: A, b: B)
  val either = Or(1, "two")
  val humanDesc = either match {
    case number Or string => s"$number is written as $string"
  }
  println(humanDesc)

  object Helper {
    def printAll(strings: String*):Unit = {
      strings.foreach(print)
    }

    def println(strings: String*): Unit = {
      strings.foreach(Predef.println)
    }
  }
  Helper.println("Hola", "Que hace capo", "Anda pa alla, bobo")
  val numbers = List(1,2,3)
  println()
  val badMatchList = (x: List[Int]) => x match {
    case List(1, _*) => s"Starting with one"
  }
  println(badMatchList(numbers))
//  println(badMatchList(List(2,3,4)))

  abstract class MyList[+A] {
    def head: A = ???
    def tail : MyList[A] = ???

//    def toSeq: Seq[A] = Seq(head) ++ tail.toSeq
  }

  case object Empty extends MyList[Nothing]
  case class Cons[+A](override val head: A, override val tail: MyList[A]) extends MyList[A]

  object MyList {
    def unapplySeq[A](list: MyList[A]) : Option[Seq[A]] = {
      val x = if (list == Empty) Some(Seq.empty)
      else unapplySeq(list.tail).map(list.head +: _)
      println(x)
      x
    }
  }

  val myList = Cons[Int](1, Cons(2, Cons(3, Empty)))
//  val myList = Cons[Int](1, Cons(2, Empty))
  println(myList)
  val matched = myList match {
    case MyList(1,2, _*) => "starts with 1, 2"
    case _ => "Everything else"
  }
  println(matched)
//  println(myList.toSeq)

  //Custom return types for unapply
  /*
  Don't have to be Option
  Data structure needs to have to methods
  isEmpty : Boolean
  get: A
   */

  abstract class Wrapper[T] {
    def isEmpty: Boolean
    def get: T
  }

  object PersonWrapper {
    def unapply(person: Person) : Wrapper[String] = new Wrapper[String] {
      override def get: String = person.name
      override def isEmpty: Boolean = false
    }
  }

  println(bob match {
    case PersonWrapper(name) => s"This is ${name}"
    case _ => "He is an alieeen"
  })


}
