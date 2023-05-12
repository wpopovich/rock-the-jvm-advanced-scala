package playground

object ScalaPlayground extends App {
  println("Hello, Scala")

  val x = 2::3::4::List(5,6)
  val y = List(5,6).::(4)
  val z = 2+:3+:4+:Seq.empty
  println(x, y, z)
}
