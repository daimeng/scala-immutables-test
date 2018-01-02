sealed abstract class MyList[+A] {
  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def ::[B >: A](h: B): MyCons[B] = MyCons(h, this)
  def lmao[B >: A](h: B): MyCons[B] = MyCons(h, this)
  override def toString: String = this match {
    case Ayy => ""
    case MyCons(h, Ayy) => h.toString
    case MyCons(h, tail) => h.toString ++ ", " ++ tail.toString
  }
}

case object Ayy extends MyList[Nothing] {
  def head = throw new NoSuchElementException("Head Empty")
  def tail = throw new NoSuchElementException("Tail Empty")
  def isEmpty: Boolean = true
}

case class MyCons[A](head: A, tail: MyList[A] = Ayy) extends MyList[A] {
  def isEmpty: Boolean = false
}

object Run extends App {
  var test1: MyList[Int] = MyCons(1, MyCons(2, MyCons(3, Ayy)))
  var test2: MyList[Int] = 1 :: 2 :: 3 :: Ayy
  var test3: MyList[Int] = Ayy lmao 3 lmao 2 lmao 1
  println(test1.toString)
  println(test2.toString)
  println(test3.toString)
}
