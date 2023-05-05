package playground

object ScalaPlayground extends App {
  println("Hello, Scala")

  val x = 2::3::4::List(5,6)
  val y = List(5,6).::(4)
  println(x, y)
}
