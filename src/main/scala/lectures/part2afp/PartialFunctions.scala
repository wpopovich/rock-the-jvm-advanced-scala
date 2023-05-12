package lectures.part2afp

import scala.util.Try

object PartialFunctions extends App {

  val aFunction = (x: Int) => x + 1

  val aPartialFunction : PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }

  println(aPartialFunction(2))

  println(aPartialFunction.lift(2))
  println(aPartialFunction.lift(999))

  val pfChain = aPartialFunction.orElse[Int, Int] {
    case 42 => 69
  }
  println(pfChain(42))
  println(pfChain.lift(69))

  /*
  Exercises
   1- Construct a PArtial function (anonymus class)
   2- dumb chatbot as a function
   */

//  def firstExercise : PartialFunction[String, String] = {
//    case "english" => "A Help menu"
//    case "spanish" => "Un menu de ayuda"
//  }
  val firstExercise = new PartialFunction[String, String] {
  override def isDefinedAt(x: String): Boolean =
    x == "english" || x == "spanish"

  override def apply(v1: String): String = v1 match {
    case "english" => "A Help menu"
    case "spanish" => "Un menu de ayuda"
  }
}
  println(firstExercise("english"))
  println(firstExercise.isDefinedAt("french"))

  val chatBot :  PartialFunction[String, Unit] = {
    case "help" =>
      println(s"""
         |Available Commands
         |help: Show this menu
         |run: Runs the program
         |exit: quits
         |play: Plays music
         |""".stripMargin)
    case "play" => println("Playing music")
    case "stop" => println("Stopping music")
    case "run" => println("Running program")
  }

  scala.io.Source.stdin.getLines().foreach(chatBot)
}
