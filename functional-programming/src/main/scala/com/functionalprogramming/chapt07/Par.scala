package com.functionalprogramming.chapt07

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Par {

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(eventIfRunning: Boolean): Boolean = false
  }

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = _ => UnitFuture(a)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    es => {
      val vala = a(es)
      val valb = b(es)
      UnitFuture(f(vala.get, valb.get))
    }

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    override def call = a(es).get
  })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sequence[A](ps: Seq[Par[A]]): Par[Seq[A]] =
    ps.foldRight(unit(Seq[A]()))((parA, parListB) => map2(parA, parListB)(_ +: _))

  def parMap[A, B](ps: Seq[A])(f: A => B): Par[Seq[B]] = fork{
    val fbs: Seq[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    map(sequence(as.map(asyncF(a => if (f(a)) List(a); else List()))))(_.toList.flatten)
  }

  def reduce[A, B](as: Seq[A])(z: Seq[A] => B, f: (B, B) => B): Par[B] =
    if (as.length <= 1)
      unit(z(as))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(fork(reduce(l)(z, f)), reduce(r)(z, f))(f)
    }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    reduce(ints)(as => as.headOption getOrElse 0, _ + _)

  def max(ints: IndexedSeq[Int]): Par[Int] =
    reduce(ints)(as => as.headOption getOrElse 0, (a, b) => a.max(b))

  def countWords(as: List[String]): Par[Int] = {
    def wordsInParagraph = (p: String) => p.split(" ").size
    reduce(as)(_.headOption.map(wordsInParagraph) getOrElse 0, _ + _)
  }

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] =
    map2(
      map2(a, b)((a, b) => (c: C) => f(a, b, c)),
      c)(_(_))

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es =>
      run(es)(choices(run(es)(n).get))

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(c => if (c) 0; else 1))(List(t, f))

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es =>
      run(es)(choices(run(es)(key).get))

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es =>
      run(es)(choices(run(es)(pa).get))

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    chooser(a)(f)

  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(c => if (c) t; else f)

  def choiceNViaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(choices)

  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(run(es)(a).get())

  def flatMapViaJoin[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    join(map(a)(f))

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(identity)

}
