package com.functionalprogramming.chapt10

import com.functionalprogramming.chapt03.{Branch, Leaf, Tree}

trait Foldable[F[_]] {

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(m: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](fa: F[A]): List[A] =
    foldRight(fa)(List.empty[A])(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(m: Monoid[B]): B =
    foldLeft(as)(m.zero)((b, a) => m.op(b, f(a)))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {

  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(m: Monoid[B]): B =
    Monoid.foldMapV(as, m)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: Stream[A])(f: A => B)(m: Monoid[B]): B =
    foldLeft(as)(m.zero)((b, a) => m.op(b, f(a)))
}

object TreeFoldable extends Foldable[Tree] {
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
    as match {
      case Leaf(a) => f(a, z)
      case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f)
    }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
    as match {
      case Leaf(a) => f(z, a)
      case Branch(left, right) => foldLeft(right)(foldLeft(left)(z)(f))(f)
    }

  override def foldMap[A, B](as: Tree[A])(f: A => B)(m: Monoid[B]): B =
    as match {
      case Leaf(a) => f(a)
      case Branch(left, right) => m.op(foldMap(left)(f)(m), foldMap(right)(f)(m))
    }
}

object OptionFoldable extends Foldable[Option] {
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: Option[A])(f: A => B)(m: Monoid[B]): B =
    as.fold(m.zero)(a => f(a))
}