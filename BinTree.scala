sealed trait BinTree[+A] {}
case class Node[A](v: A, l: BinTree[A], r: BinTree[A]) extends BinTree[A]
case class Leaf[A](v: A) extends BinTree[A]
case object Empty extends BinTree[Nothing]

object Run extends App {
  println("BinTree")
}