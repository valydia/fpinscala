package fpinscala.datastructures

import fpinscala.gettingstarted.MyModule.fib

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val value: Int = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => throw new NoSuchElementException
      case Cons(_, tail) => tail
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => throw new NoSuchElementException
      case Cons(_, tail) => Cons(h,tail)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    (l,n) match {
      case (_, 0) => l
      case (Nil, _) => Nil
      case (Cons(_, tail), _) => drop(tail, n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h,t) => if (f(h)) dropWhile(t,f) else t
      case _ => l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => throw new NoSuchElementException
      case Cons(_, Nil) => Nil
      case Cons(x1, Cons(x2, xs)) => Cons(x1, Cons(x2, init(xs)))
    }

  def length[A](l: List[A]): Int =
    foldRight(l,0)((_, acc) => acc + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h, tail) => foldLeft(tail,f(z,h))(f)
    }

  def map[A,B](l: List[A])(f: A => B): List[B] = {
     def loop(l: List[A], acc: List[B]): List[B] = l match {
       case Nil => reverse(acc)
       case Cons(h,t) => loop(t, Cons(f(h),acc))
     }
    loop(l, List())
  }


  def map2[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, List[B]())((a,b) => Cons(f(a), b))

  def filter[A](l: List[A])(p: A => Boolean): List[A] = {
    def loop(l: List[A], acc: List[A]): List[A] = l match {
      case Nil => reverse(acc)
      case Cons(h,t) => loop(t, if (p(h)) Cons(h,acc) else acc)
    }
    loop(l, List())
  }

  def filter2[A](l: List[A])(p: A => Boolean): List[A] =
    flatMap(l)(a => if (p(a)) List(a) else List())

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    def loop(l: List[A], acc: List[B]): List[B] = l match {
      case Nil => reverse(acc)
      case Cons(h,t) => loop(t, append(f(h),acc))
    }
    loop(as, List())
  }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil:List[A])((a,b) => Cons(b,a))

  def append2[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1,l2){(elem,acc) =>
      Cons(elem,acc)
    }

  def flatten[A](list: List[List[A]]): List[A] =
    foldRight(list, List[A]()){ (subList, acc) =>
      append(subList, acc)
    }

  def addOne(l: List[Int]): List[Int] = map(l)(_ + 1)
  def toString(l: List[Double]): List[String] = map(l)(_.toString)

  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A,B) => C): List[C] = {
    def loop(l1: List[A],l2: List[B], acc: List[C]): List[C] = (l1,l2) match {
      case (Nil,_) => reverse(acc)
      case (_, Nil) => reverse(acc)
      case (Cons(h1,t1), Cons(h2,t2)) => loop(t1,t2,Cons(f(h1,h2), acc))
    }
    loop(l1,l2, List())
  }

  def startWiths[A](l: List[A], prefix: List[A]): Boolean = {
    @tailrec
    def loop(l: List[A], p: List[A]): Boolean =
      (l,p) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case (Cons(h1,t1), Cons(h2,t2)) => if (h1 == h2) loop(t1,t2) else false
      }


    loop(l,prefix)
  }
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def loop(l: List[A]): Boolean = l match {
      case Nil => false
      case Cons(_,t) => if (startWiths(l,sub)) true else loop(t)
    }
    loop(sup)
  }


  def main(args: Array[String]): Unit = {

    val list = List(1,2,3,4,5)
//        println(s"Working with list ${list}")
//    println(s"tail -- ${tail(list)}")
//    println(s"setHead -- ${setHead(list, 10)}")
//    println(foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
//    println(reverse(list))

//    println(append2(list, List(6,7,8,9,10)))
//    println(flatten(List(List(1,2,3,4),List(5,6,7), List(8,9,10))))
//    println(filter2(list)(_ % 2 == 0))
//    println(flatMap(List(1,2,3))(i => List(i,i)))
//    println(zipWith(List(1,2,3), List(4,5,6))(_ + _))
    println(hasSubsequence(List(1,2,3,4), List(1,2)))
    println(hasSubsequence(List(1,2,3,4), List(2,3)))
    println(hasSubsequence(List(1,2,3,4), List(4)))
    println(hasSubsequence(List(1,2,3,4), List(3,2)))
  }

}
