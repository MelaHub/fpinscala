package com.functionalprogramming.chapt03

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] =
    as match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }

  def setHead[A](x: A, xs: List[A]): List[A] =
    xs match {
      case Nil => Nil
      case Cons(_, t) => Cons(x, t)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }


  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, acc) => 1 + acc)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sumFoldLeft(ints: List[Int]): Int =
    foldLeft(ints, 0)(_ + _)

  def productFoldLeft(ds: List[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)

  def lengthFoldLeft[A](as: List[A]): Int =
    foldLeft(as, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((x, y) => Cons(y, x))

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((x, y) => f(y, x))

  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def append[A](as1: List[A], as2: List[A]): List[A] =
    foldRight(as1, as2)(Cons(_, _))

  def concat[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil: List[A])(append)

  def addsOne(as: List[Int]): List[Int] =
    foldRight(as, Nil: List[Int])((h, t) => Cons(h + 1, t))

  def double2String(as: List[Double]): List[String] =
    foldRight(as, Nil: List[String])((h, t) => Cons(h.toString, t))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])(
      (h, t) =>
        if (f(h)) Cons(h, t)
        else t
    )

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) Cons(x, Nil) else Nil)

  def addLists(as1: List[Int], as2: List[Int]): List[Int] =
    (as1, as2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addLists(t1, t2))
      case _ => Nil
    }

  def zipWith[A, B, C](as1: List[A], as2: List[B])(implicit f: (A, B) => C): List[C] =
    (as1, as2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2))
      case _ => Nil
    }

  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = {
    def go(sup1: List[A], sub1: List[A], sequenceStarted: Boolean): Boolean =
      (sup1, sub1) match {
        case (_, Nil) => true
        case (Cons(h1, t1), Cons(h2, t2)) =>
          if (h1 == h2) go(t1, t2, true)
          else {
            if (sequenceStarted) false
            else go(t1, sub1, false)
          }
        case _ => false
      }
    go(sup, sub, false)
  }

}
