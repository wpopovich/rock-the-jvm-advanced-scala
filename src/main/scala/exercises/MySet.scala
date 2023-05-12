package exercises

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean){
  /*
  Exercise: Implement a functional set
   */
  def apply(elem: A): Boolean =
    contains(elem)
  def contains(elem: A) : Boolean
  def +(elem: A) : MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]
  def map[B](f: A => B) : MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean) : MySet[A]
  def foreach(f: A => Unit): Unit

  /*
    Exercises
    Removing an element
    intersection with another set
    difference with another set
     */
  def -(elem: A) : MySet[A]
  def &(otherSet: MySet[A]) : MySet[A]
  def --(otherSet: MySet[A]): MySet[A]

  /*
  Excercise 3 Implement unary_! = Negation of a Set
  set[1,2,3]
   */
  def unary_! : MySet[A]
}

class EmptySet[A]() extends MySet[A] {
  override def contains(elem: A): Boolean = false

  override def +(elem: A): MySet[A] = new NonEmptySet[A](elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  override def map[B](f: A => B): MySet[B] = new EmptySet[B]

  override def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]

  override def filter(predicate: A => Boolean): MySet[A] = this

  override def foreach(f: A => Unit): Unit = ()

  override def -(elem: A): MySet[A] = this

  override def &(otherSet: MySet[A]): MySet[A] = this

  override def --(otherSet: MySet[A]): MySet[A] =
    //shouldn't it be ottherSet?
    this

  override def unary_! : MySet[A] = new PropertyBasedSet[A](_ => true)
}
class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  override def contains(elem: A): Boolean = {
    head == elem || tail.contains(elem)
  }

  override def +(elem: A): MySet[A] = {
    if (this.contains(elem)) this
    else new NonEmptySet[A](elem, this)
  }

  override def ++(anotherSet: MySet[A]): MySet[A] =
    tail ++ anotherSet + head

  override def map[B](f: A => B): MySet[B] =
    tail.map(f) + f(head)

  override def flatMap[B](f: A => MySet[B]): MySet[B] =
    tail.flatMap(f) ++ f(head)

  override def filter(predicate: A => Boolean): MySet[A] =
    if (predicate(head)) tail.filter(predicate) + head
    else tail.filter(predicate)

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  override def -(elem: A): MySet[A] =
    if (head == elem) tail
    else tail - elem + head
  override def --(otherSet: MySet[A]): MySet[A] = {
//    if (!otherSet.contains(head)) MySet(head) ++ (tail -- otherSet) ++ (otherSet -- tail)
//    else tail -- otherSet ++
    filter(x => !otherSet(x))
  }

  override def &(otherSet: MySet[A]): MySet[A] = {
    filter(otherSet)
//    if (otherSet.contains(head)) MySet(head) ++ (tail & otherSet)
//    else tail & otherSet
  }

  override def unary_! : MySet[A] = new PropertyBasedSet[A](x => !this.contains(x))
}

class PropertyBasedSet[A](property: A => Boolean) extends MySet[A] {
  override def contains(elem: A): Boolean = property(elem)
  // {x in A | property(x)} + element = {x in A | property(x) || x == element}
  override def +(elem: A): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || elem == x)

  override def ++(anotherSet: MySet[A]): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || anotherSet(x))

  override def map[B](f: A => B): MySet[B] = politelyFail

  override def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail

  override def foreach(f: A => Unit): Unit = politelyFail
  override def filter(predicate: A => Boolean): MySet[A] =
    new PropertyBasedSet[A](x => property(x) && predicate(x))

  override def -(elem: A): MySet[A] = filter(x => x != elem)

  override def &(otherSet: MySet[A]): MySet[A] = filter(otherSet)

  override def --(otherSet: MySet[A]): MySet[A] = filter(!otherSet)

  override def unary_! : MySet[A] = new PropertyBasedSet[A](x => !property(x))

  private def politelyFail = throw new IllegalArgumentException("Not Supported")
}

object MySet {
  def apply[A](elems: A*): MySet[A] = {
    @tailrec
    def buildSet(elemSeq: Seq[A], acc: MySet[A]): MySet[A] = {
      if (elemSeq.isEmpty) acc
      else buildSet(elemSeq.tail, acc + elemSeq.head)
    }
    buildSet(elems.toSeq, new EmptySet[A])
  }
}

object MySetApp extends App {
  val mySet = MySet(1,2,3,4)
  val otherSet = MySet(3,4,5,6)
  val emptySet = new EmptySet[Int]
//  mySet.foreach(println)
//  mySet + 6 foreach(println)
//  mySet ++ MySet(-1,-2) + 3 foreach(println)
//  mySet.map(_ * 2) foreach(println)
//  mySet.flatMap(e => MySet(e, e * 10)) foreach(println)
//  mySet.filter(_ % 2 == 0) foreach(println)
//  mySet - 4 foreach(println)
//mySet & otherSet foreach(println)
//mySet -- otherSet foreach(println)
  val negative = !mySet // should contain numbers greater than 4
  println(negative(2))
  println(negative(5))

  val negativeEven = negative.filter(_ % 2 == 0)
  println(negativeEven(6))
  println(negativeEven(5))

  println((negativeEven + 5)(5))
}



