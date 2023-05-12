package exercises

trait MySeq[A] extends PartialFunction[Int, A] {
  def contains(elem: A) : Boolean

}

class EmptySeq[A] extends MySeq[A] {
  override def contains(elem: A): Boolean = ???
  override def isDefinedAt(x: Int): Boolean = ???

  override def apply(v1: Int): A = ???
}
