package com.functionalprogramming.chapt10

import com.functionalprogramming.chapt07.Par
import com.functionalprogramming.chapt07.Par.Par
import com.functionalprogramming.chapt08.{Gen, Prop}
import com.sun.rowset.internal.WebRowSetXmlReader

trait Monoid[A] {

  self =>

  def op(a1: A, a2: A): A
  def zero: A

  def dual = new Monoid[A] {
    def op(a1: A, a2: A): A = self.op(a2, a1)
    def zero: A = self.zero
  }

}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    def zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    def zero = Nil
  }

  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    def zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    def zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    def zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    def zero = true
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
    def zero = None
  }

  def endoMonoid[A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A) = a1 andThen a2
    def zero = identity
  }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid[B].dual)(a => b => f(b, a))(z)

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.length == 0) {
      m.zero
    } else if (v.length == 1) {
      f(v(0))
    } else {
      val (l, r) = v.splitAt(v.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {

    override def op(a1: Par[A], a2: Par[A]): Par[A] =
      Par.map2(a1, a2)(m.op)

    override def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.flatMap(Par.parMap(v)(f)) {
      parSeq =>
        foldMapV(parSeq.toIndexedSeq, par(m))(el => Par.lazyUnit(el))
    }

  def ordered[A](v: IndexedSeq[A])(implicit ordering: Ordering[A]) = {
    import ordering._
    val isOrderedMonoid = new Monoid[Option[(A, A, Boolean)]] {
      def op(a1: Option[(A, A, Boolean)], a2: Option[(A, A, Boolean)]) =
        (a1, a2) match {
          case (Some((minA1, maxA1, validA1)), Some((minA2, maxA2, validA2))) if validA1 && validA2 && minA2 > maxA1 => Some((minA1, maxA2, true))
          case (Some((minA1, maxA1, validA1)), Some((minA2, maxA2, validA2))) =>
            Some(minA1, maxA2, false)
          case (x, None) => x
          case (None, x) =>x
        }

      override def zero: Option[(A, A, Boolean)] = None
    }

    foldMapV(v, isOrderedMonoid)(el => Some(el, el, true))
      .map(_._3)
      .getOrElse(true)

  }

  sealed trait WC

  case class Stub(chars: String) extends WC

  object Stub {
    def empty = Stub("")
  }

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {

    def op(a1: WC, a2: WC) =
      (a1, a2) match {
        case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
        case (Stub(s1), Part(lStub, words, rStub)) => Part(s1 + lStub, words, rStub)
        case (Part(lStub, words, rStub), Stub(s2)) => Part(lStub, words, rStub + s2)
        case (Part(lStub1, w1, rStub1), Part(lStub2, w2, rStub2)) =>
          Part(lStub1, w1 + w2 + (if ((rStub1 + lStub2).isEmpty) 0; else 1), rStub2)
      }

    def zero = Stub.empty
  }

  def countWords(inputString: String): Int = {
    foldMapV(inputString.toIndexedSeq, wcMonoid)(
      c =>
        if (c.isSpaceChar) Part("", 0, "")
        else Stub(c.toString)
    ) match {
      case Stub(_) => 1
      case Part(left, wCount, right) => wCount + (if (left.isEmpty) 0; else 1) + (if (right.isEmpty) 0; else 1)
    }

  }

  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {

    override def op(a1: (A, B), a2: (A, B)): (A, B) = (a.op(a1._1, a2._1), b.op(a1._2, a2._2))

    override def zero: (A, B) = (a.zero, b.zero)
  }

  def mapMergeMonoid[K, V](v: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {

    override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
      (a1.keySet ++ a2.keySet).foldLeft(zero) {
        (acc, k) => acc.updated(k, v.op(a1.getOrElse(k, v.zero), a2.getOrElse(k, v.zero)))
      }

    override def zero: Map[K, V] = Map()
  }

  def functionMonoid[A, B](b: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {

    override def op(a1: A => B, a2: A => B): A => B =
      (a: A) => b.op(a1(a), a2(a))

    override def zero: A => B = _ => b.zero
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))


}
