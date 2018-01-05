import scala.annotation.tailrec

sealed trait StackQueue[+A] {
  def isEmpty: Boolean

  def push[B >: A](v: B): StackQueue[B] = this match {
    case Empty => NonEmpty(v :: Nil, Nil)
    case NonEmpty(in, out) => NonEmpty(v :: in, out)
  }

  def pop[B >: A]: (B, StackQueue[B]) = _peek

  def peek: Option[A] = if (this.isEmpty) None else Some(_peek._1)

  private lazy val _peek: (A, StackQueue[A]) = this match {
    case Empty => throw new NoSuchElementException("Empty Queue")
    case NonEmpty(Nil, o :: Nil) => (o, Empty)
    case NonEmpty(in, Nil) => {
      val r = in.reverse
      (r.head, NonEmpty(Nil, r.tail))
    }
    case NonEmpty(in, out) => (out.head, NonEmpty(in, out.tail))
  }

  lazy val toList: List[A] = this match {
    case Empty => Nil
    case NonEmpty(in, out) => in ++ out.reverse
  }

  override def toString: String = {
    @tailrec
    def loop(list: List[A], s: String): String = list match {
      case h :: Nil => s ++ h.toString
      case h :: tail => loop(tail, s ++ h.toString ++ "->")
      case Nil => ""
    }
    loop(toList, "")
  }
}

case class NonEmpty[A](in: List[A], out: List[A]) extends StackQueue[A] {
  def isEmpty: Boolean = false
}
// case class In[A](in: List[A]) extends StackQueue[A]
// case class Out[A](out: List[A]) extends StackQueue[A]

case object Empty extends StackQueue[Nothing] {
  def isEmpty: Boolean = true
}

object Run extends App {
  var q: StackQueue[Int] = Empty
  q = q.push(1) // NonEmpty(1)
  q = q.push(2) // NonEmpty(2::1)
  val q2 = q.push(3) // NonEmpty(3::2::1)
  val (v, q3) = q2.pop // (1, NonEmpty(Nil, 3::2))
  println(q2) // 3->2->1
  println(q3) // 3->2
  println(v) // 1
}