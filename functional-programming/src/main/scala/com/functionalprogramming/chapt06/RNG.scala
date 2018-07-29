package com.functionalprogramming.chapt06

trait RNG {
  def nextInt: (Int, RNG)

}

object RNG {

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (thisVal, nextRNG) = rng.nextInt
    if (thisVal < 0)
      (-(thisVal + 1), nextRNG)
    else
      (thisVal, nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (thisVal, nextRNG) = nonNegativeInt(rng)
    (thisVal / (Int.MaxValue.toDouble + 1), nextRNG)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (intVal, nextRNG) = rng.nextInt
    val (doubleVal, nextRNG2) = double(nextRNG)
    ((intVal, doubleVal), nextRNG2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((intVal, doubleVal), nextRNG) = intDouble(rng)
    ((doubleVal, intVal), nextRNG)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, nextRNG) = double(rng)
    val (d2, nextRNG2) = double(nextRNG)
    val (d3, nextRNG3) = double(nextRNG2)
    ((d1, d2, d3), nextRNG3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(n: Int, currRNG: RNG, acc: List[Int]): (List[Int], RNG) = {
      if (n > 0) {
        val (v, nextRNG) = currRNG.nextInt
        go(n - 1, nextRNG, v :: acc)
      } else {
        (acc, currRNG)
      }
    }
    go(count, rng, List.empty)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - 1 % 2)

  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(d => d / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rnd => {
      val (a, rnd1) = ra(rnd)
      val (b, rnd2) = rb(rnd1)
      (f(a, b), rnd2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A]))((rng, acc) => map2(rng, acc)(_ :: _))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeLessThen(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)({
      i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(mod)
        else nonNegativeLessThen(n)
    })

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  case class State[S, +A](run: S => (A, S)) {
    import State._

    def flatMap[B](f: A => State[S, B]): State[S, B] = State(
      s => {
        val (a, s1) = run(s)
        f(a).run(s1)
      }
    )

    def map[B](f: A => B): State[S, B] =
      flatMap(a => State.unit(f(a)))
    def map2ViaFlatMap[B, C](bs: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => bs.map(b => f(a, b)))
  }

  object State {

    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
      fs.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2ViaFlatMap(acc)( _ :: _ ))

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield ()

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  }

}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

}
