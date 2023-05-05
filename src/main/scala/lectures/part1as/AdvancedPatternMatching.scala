package lectures.part1as

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
}
