package FPS.errorhandling

import FPS.datastructures.{List, Cons, Nil}

/**
  * Created by luca.catallo.293@gmail.com on 09/08/2016.
  */

sealed trait Option[+A] {
  def filter(f: A => Boolean): Option[A] = Option.filter(this)(f)
  def flatMap[B](f: A => Option[B]): Option[B] = Option.flatMap(this)(f)
  def getOrElse[B >: A](default: => B): B = Option.getOrElse(this, default)
  def map[B](f: A => B): Option[B] = Option.map(this)(f)
  def orElse[B >: A](default: => Option[B]): Option[B] = Option.orElse(this, default)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  // 4.1
  def map[A,B](a: Option[A])(f: A => B): Option[B] = a match {
    case Some(x) => Some(f(x))
    case _ => None
  }

  def getOrElse[A](a: Option[A], default: => A): A = a match {
    case Some(x) => x
    case _ => default
  }

  def flatMap[A,B](a: Option[A])(f: A => Option[B]): Option[B] =
    getOrElse(map(a)(f), None)

  def orElse[A](a: Option[A], default: => Option[A]): Option[A] =
    getOrElse(map(a)(Some(_)), default)

  def filter[A](a: Option[A])(f: A => Boolean): Option[A] =
    flatMap(a)(x => if (f(x)) Some(x) else None)

  // 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    val mean: Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum/xs.length)
    mean.flatMap(m => Some(xs.map(x => math.pow(x - m, 2)).sum/xs.length))
  }

  // 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa,bb)))

  // 4.4
  //  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
  //    case Nil => Some(Nil)
  //    case x :: t => x.flatMap(x => sequence(t).map(x :: _))
  //  }
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    @annotation.tailrec
    def loop(a: List[Option[A]], acc: Option[List[A]]): Option[List[A]] =
      a match {
        case Cons(x,t) => x match {
          case None => None
          case Some(x) => loop(t, acc.map(x :: _))
        }
        case _ => acc
      }

    loop(a, Some(Nil)).map(_.reverse)
  }

  //4.5
  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    @annotation.tailrec
    def loop(a: List[A], acc: Option[List[B]]): Option[List[B]] =
      a match {
        case Cons(x,t) => f(x) match {
          case None => None
          case Some(x) => loop(t, acc.map(x :: _))
        }
        case _ => acc
      }

    loop(a, Some(Nil)).map(_.reverse)
  }
  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

}