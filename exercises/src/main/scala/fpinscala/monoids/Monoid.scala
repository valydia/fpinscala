package fpinscala.monoids

import fpinscala.monoids.ListFoldable.foldRight
import fpinscala.monoids.Monoid.concatenate
import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps

import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    val zero: Int = 0
    def op(a1: Int, a2: Int): Int = a1 + a2
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    val zero: Int = 1
    def op(a1: Int, a2: Int): Int = a1 * a2
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def zero: Boolean = false

    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def zero: Boolean = true

    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def zero: Option[A] = None

    override def op(a1: Option[A], a2: Option[A]): Option[A] =
      a1 orElse a2

  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {

    override def zero: A => A = identity

    override def op(a1: A => A, a2: A => A): A => A =
      a1 andThen a2
  }

  def endoMonoidDual[A]: Monoid[A => A] = new Monoid[A => A] {

    override def zero: A => A = identity

    override def op(a1: A => A, a2: A => A): A => A =
      a1 compose a2
  }

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
  trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    concatenate(as.map(f), m)

  def swapParameter[A,B,C](f:(A, B) => C): (B,A) => C =
    (b,a) => f(a,b)

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap[A, B => B](as,endoMonoid[B])(f.curried)(z)

  def main(args: Array[String]): Unit = {
    println("hello world")
    println(ordered((1 to 10).toVector))
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap[A,B => B](as, endoMonoidDual)(a => b => f(b,a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = as.length match {
    case 0 => m.zero
    case 1 => f(as.head)
    case _ =>
      val (beginning, end) = as.splitAt(as.length / 2)
      val b1 = foldMapV(beginning,m)(f)
      val b2 = foldMapV(end,m)(f)
      m.op(b1,b2)
  }

  // This implementation detects only ascending order,
  // but you can write a monoid that detects both ascending and descending
  // order if you like.
  def ordered(ints: IndexedSeq[Int]): Boolean = {
    // Our monoid tracks the minimum and maximum element seen so far
    // as well as whether the elements are so far ordered.
    val mon = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]) =
        (o1, o2) match {
          // The ranges should not overlap if the sequence is ordered.
          case (Some((x1, y1, p)), Some((x2, y2, q))) =>
            println(s"x1 $x1, y1 $y1 , x2 $x2, y2 $y2")
            Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
          case (x, None) => x
          case (None, x) => x
        }
      val zero = None
    }
    // The empty sequence is ordered, and each element by itself is ordered.
    val x = foldMapV[Int,Option[(Int, Int, Boolean)]](ints, mon)(i => Some((i, i, true)))
      x.forall(_._3)
  }

  sealed trait WC {
    def length: Int
  }
  case class Stub(chars: String) extends WC {
    def length: Int = chars.length
  }
  case class Part(lStub: String, words: Int, rStub: String) extends WC {
    def length: Int = lStub.length + words + rStub.length
  }

  object WC {
    def apply(s: String): WC = ???
  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = 
    ???

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = 
    ???

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override val zero: WC = Stub("")

    override def op(a1: WC, a2: WC): WC =
      (a1,a2) match {
        case (Stub(w1),Stub(w2)) => Stub(w1 + w2)
        case (Stub(w1),Part(lStub, w, rStub)) => Part(w1 + lStub, w, rStub)
        case (Part(lStub, w, rStub), Stub(w1)) => Part(lStub, w, rStub + w1)
        case (Part(lStub1, w1, rStub1), Part(lStub2, w2, rStub2)) =>
          val merged = if ((rStub1 + lStub2).isEmpty) 0 else 1
          Part(lStub1, w1 + merged + w2, rStub2)
      }
  }

  def count(s: String): Int = {
    ???
  }

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def zero: (A, B) = (A.zero, B.zero)

      def op(a1: (A, B), a2: (A, B)): (A, B) = {
        val (a,b) = a1
        val (aa,bb) = a2
        (A.op(a,aa), B.op(b, bb))
      }
    }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      def zero: A => B = _ => B.zero

      def op(f1: A => B, f2: A => B): A => B =
        a => B.op(f1(a), f2(a))
    }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero: Map[K,V] = Map[K,V]()
      def op(a: Map[K, V], b: Map[K, V]): Map[K,V] =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero),
            b.getOrElse(k, V.zero)))
        }
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    ???
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] = foldRight(as)(List[A]())(_ :: _)
}

object ListFoldable extends Foldable[List] {
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    ListFoldable.concatenate(as.map(f))(mb) //is it the most efficient?

  override def toList[A](as: List[A]): List[A] = as

}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((b,a) => mb.op(b, f(a)))

}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
    foldLeft(as)((b:B) => b)((g,a) => b => g(f(a,b)))(z)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B = ???
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](tree: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    tree match {
      case Leaf(a) => f(a)
      case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
    }
  override def foldLeft[A, B](tree: Tree[A])(z: B)(f: (B, A) => B): B =
    tree match {
      case Leaf(a) => f(z, a)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }
  override def foldRight[A, B](tree: Tree[A])(z: B)(f: (A, B) => B): B =
    tree match {
      case Leaf(a) => f(a, z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as.fold(mb.zero)(a => f(a))
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
    as match {
      case None => z
      case Some(a) => f(z,a)
    }
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
    as match {
      case None => z
      case Some(a) => f(a,z)
    }
}

