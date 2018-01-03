sealed trait BinTree[+A] {
  def v: A
  def l: BinTree[A]
  def r: BinTree[A]

  def :-[B >: A](v: Node[B]): Node[B] = this match {
    case Empty => v
    case n: Node[A] => Node(n.v, n.l, v)
  }

  def -:[B >: A](v: Node[B]): Node[B] = this match {
    case Empty => v
    case n: Node[A] => Node(n.v, v, n.r)
  }

  override def toString: String = this match {
    case Empty => ""
    case Node(v, Empty, Empty) => v.toString
    case Node(v, Empty, r) => "(" ++ v.toString ++ ":-" ++ r.toString ++ ")"
    case Node(v, l, Empty) => "(" ++ l.toString ++ "-:" ++ v.toString ++ ")"
    case Node(v, l, r) => "(" ++ l.toString ++ "-:" ++ v.toString ++ ":-" ++ r.toString ++ ")"
  }
}

case class Node[A](v: A, l: BinTree[A] = Empty, r: BinTree[A] = Empty) extends BinTree[A]
case object Empty extends BinTree[Nothing] {
  def v = throw new NoSuchElementException("No value")
  def l = throw new NoSuchElementException("No left")
  def r = throw new NoSuchElementException("No right")
}

object Run extends App {
  val t: BinTree[Int] = Node(4, Node(1), Node(2, Empty, Node(3)))
  val n = Node
  var t2: BinTree[Int] = (n(1) -: n(4) :- (n(2) :- n(3)))
  println(t.toString)  //-> (1-:4:-(2:-3))
  println(t2.toString) //-> (1-:4:-(2:-3))
}
