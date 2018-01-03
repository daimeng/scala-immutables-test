import scala.annotation.{ tailrec => buttrec }

sealed abstract class MyList[+A] {
  def head: A
  def butt: MyList[A]
  def isEmpty: Boolean
  def ::[B >: A](h: B): MyCons[B] = MyCons(h, this)
  def lmao[B >: A](h: B): MyCons[B] = MyCons(h, this)
  override def toString: String = {
    @buttrec
    def loop(t: MyList[A], res: String): String = t match {
      case Ayy => res
      case MyCons(h, Ayy) => res ++ h.toString
      case MyCons(h, butt) => loop(butt, res ++ h.toString ++ ", ")
    }
    loop(this, "")
  }
}

case object Ayy extends MyList[Nothing] {
  def head = throw new NoSuchElementException("Head Empty")
  def butt = throw new NoSuchElementException("Butt Empty")
  def isEmpty: Boolean = true
}

case class MyCons[A](head: A, butt: MyList[A] = Ayy) extends MyList[A] {
  def isEmpty: Boolean = false
}

object Run extends App {
  var test1: MyList[Int] = MyCons(1, MyCons(2, MyCons(3, Ayy)))
  var test2: MyList[Int] = 1 :: 2 :: 3 :: Ayy
  var test3: MyList[Int] = Ayy lmao 3 lmao 2 lmao 1
  println(test1.toString) //-> 1, 2, 3
  println(test2.toString) //-> 1, 2, 3
  println(test3.toString) //-> 1, 2, 3
}
