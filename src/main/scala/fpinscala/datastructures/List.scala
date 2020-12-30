package fpinscala.datastructures

import scala.annotation.tailrec

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

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  // 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  // 3.3
  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => Cons(a, t)
  }

  // 3.4
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case l if n <= 0 => l
    case Cons(_, t) => drop(t, n - 1)
  }

  // 3.5
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case l => l
  }

  // 3.6
  def init[A](l: List[A]): List[A] = {
    def go(acc: List[A], l: List[A]): List[A] = l match {
      case Cons(_, Nil) => acc
      case Cons(h, t) => go(Cons(h, acc), t)
    }

    def reverse(acc: List[A], l: List[A]): List[A] = l match {
      case Cons(h, Nil) => Cons(h, acc)
      case Cons(h, t) => reverse(Cons(h, acc), t)
    }
    reverse(Nil, go(Nil, l))
  }

  def init2[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }
}