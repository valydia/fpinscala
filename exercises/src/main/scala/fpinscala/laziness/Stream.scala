package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = {
    def loop[A](s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc.reverse
      case Cons(h, t) => loop(t(), h():: acc)
    }
    loop(this,Nil)
  }

  def toList2: List[A] = this match {
    case Empty => Nil
    case Cons(h,t) => h() :: t().toList2
  }

  def take(n: Int): Stream[A] = {
    def loop(n: Int, s: Stream[A], acc: Stream[A]): Stream[A] =
      if (n == 0) acc
      else {
        s match {
          case Empty => acc
          case Cons(h, t) => cons(h(),loop(n - 1, t(), acc))
        }
      }
    loop(n, this, Empty)
  }

  def drop(n: Int): Stream[A] = {
    @annotation.tailrec
    def loop(n: Int, s: Stream[A]): Stream[A] =
        if (n == 0) s
        else {
          s match {
            case Empty => s
            case Cons(_, t) => loop(n - 1, t())
          }
        }

    loop(n, this)
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => if (p(a)) cons(a,b) else b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b )

  def headOption: Option[A] =
    foldRight[Option[A]](None)((elem, _) => Some(elem))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((elem,acc) => cons(f(elem), acc))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream[A]())((elem,acc) => if (p(elem)) cons(elem, acc) else acc)

  def append[AA >: A](s: => Stream[AA]): Stream[AA] =
    foldRight(s)((elem,acc) => cons(elem, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream[B]())((elem,acc) => f(elem) append acc)

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  val fibs: Stream[Int] =
    unfold((0,1)){case (n1, n2) => Some(n1 + n2,(n1 + n2, n1))}

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }
  }


  def main(args: Array[String]): Unit = {
    val stream = Stream(1 to 5:_*)

//    println(stream.take(7).toList)
//    println(stream.takeWhile(_ < 5).toList)
//    println((stream append Stream(6 to 10:_*)).toList)
//    println(Empty.headOption)
    println(fibs.take(10).toList)
  }
}