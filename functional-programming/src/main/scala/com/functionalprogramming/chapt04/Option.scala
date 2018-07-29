package com.functionalprogramming.chapt04


sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] =
    this match {
      case Some(value) => Some(f(value))
      case None => None
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B =
    this match {
      case Some(value) => value
      case None => default
    }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    flatMap(v => if (f(v)) Some(v) else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.length > 0) Some(xs.sum / xs.length)
    else None

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap {
      m => mean(xs.map(x => math.pow(x - m, 2)))
    }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap(v1 => b map(v2 => f(v1, v2)))

  def sequence[A](a: scala.List[Option[A]]): Option[scala.List[A]] =
    a.foldRight(Some(scala.Nil): Option[scala.List[A]])((a, b) => map2(a, b)((x, y) => x :: y))

  def traverse[A, B](a: scala.List[A])(f: A => Option[B]): Option[scala.List[B]] =
    a.foldRight(Some(scala.Nil): Option[scala.List[B]])((a, b) => map2(f(a), b)((x, y) => x :: y))

  def sequenceAsTraverse[A](a: scala.List[Option[A]]): Option[scala.List[A]] =
    traverse(a)(x => x)

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None}
}