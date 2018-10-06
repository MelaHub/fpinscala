package com.functionalprogramming.chapt11


case class Id[A](value: A) {

  def flatMap[A, B](f: A => Id[B]): Id[B] = f(value)

  def map[A, B](f: A => B): Id[B] = Id(f(value))

}
