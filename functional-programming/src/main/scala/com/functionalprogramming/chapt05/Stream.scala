package com.functionalprogramming.chapt05

import scala.annotation.tailrec

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = {
    @tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = {
      s match {
        case Empty => acc
        case Cons(h, t) => go(t(), h() :: acc)
      }
    }
    go(this, Nil).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n - 1))
    case _ => Stream.empty
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case Cons(f, t) if n == 0 => this
    case _ => Stream.empty
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => Stream.cons(h(), t().takeWhile(f))
    case _ => Stream.empty
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileFoldRight(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) =>
      if (f(a)) Stream.cons(a, b)
      else Stream.empty
    )

  def headOptionFoldRight: Option[A] =
    foldRight(Option.empty[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => Stream.cons(a, b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) =>
    if (f(a)) Stream.cons(a, b)
    else b
    )

  def flatMap[B](f: A => Stream[B]): Stream[B] =
   foldRight(Stream.empty[B])((a, b) => f(a) append b)


  def mapViaUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this){
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    Stream.unfold((n, this)) {
      case (c, Cons(h, t)) if c > 0 => Some((h(), (c - 1, t())))
      case _ => None
    }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(h, t) if f(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Stream.empty))
      case (Empty, Cons(h, t)) => Some((None, Some(h())), (Stream.empty, t()))
      case (Empty, Empty) => None
    }

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile{case (_, s2) => s2.nonEmpty}.forAll{case (a, b) => a == b}

  def tails: Stream[Stream[A]] =
    Stream.unfold(this){
      case s => s match {
        case Cons(h, t) => Option(s, t())
        case Empty => None
      }
    }

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    foldRight((z, Stream(z)))((a, b) => {
      lazy val z0 = b
      val (v, s) = z0
      val newVal = f(a, v)
      (newVal, Stream.cons(newVal, s))
    }
    )._2
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, f: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t1
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(previous: Int, current: Int): Stream[Int] =
      cons(current, go(current, previous + current))

    cons(0, go(0, 1))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }
  }

  def fibsViaUnfold =
    unfold((0,1)) { case (previous: Int, current: Int) => Some((previous, (current, previous + current))) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(s => Some((s, s + 1)))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

}


