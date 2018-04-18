package FPS.errorhandling

/**
  * Created by luca.catallo.293@gmail.com on 09/08/2016.
  */

import FPS.datastructures.{List, Cons, Nil}


sealed trait Either[+E,+A] {
  def flatMap[EE >: E,B](f: A => Either[EE,B]): Either[EE,B] = Either.flatMap(this)(f)
  def map[B](f: A => B): Either[E,B] = Either.map(this)(f)
  def map2[EE >: E,B,C](b: Either[EE, B])(f: (A,B) => C): Either[EE,C] = Either.map2(this, b)(f)
  def orElse[EE >: E, B >: A](b: => Either[EE,B]): Either[EE,B] = Either.orElse(this, b)
}
case class Left[+E](value: E) extends Either[E,Nothing]
case class Right[+A](value: A) extends Either[Nothing,A]

object Either {

  // 4.6
  def map[E,A,B](e: Either[E,A])(f: A => B): Either[E,B] =
    e match {
      case Left(e) => Left(e)
      case Right(x) => Right(f(x))
    }

  def flatMap[E,EE >: E,A,B](e: Either[E,A])(f: A => Either[EE,B]): Either[EE,B] =
    map(e)(f) match {
      case Left(e) => Left(e)
      case Right(x) => x
    }

  def orElse[E,A](a: Either[E,A], b: => Either[E,A]): Either[E,A] =
    a match {
      case Left(_) => b
      case Right(x) => Right(x)
    }

  def map2[E,A,B,C](a: Either[E,A],b: Either[E,B])(f: (A,B) => C): Either[E,C] =
    flatMap(a)(aa => map(b)(bb => f(aa,bb)))

  //4.7
  def traverse[E,A,B](l: List[A])(f: A => Either[E,B]): Either[E,List[B]] = {
    @annotation.tailrec
    def loop(l: List[A], acc: Either[E,List[B]]): Either[E,List[B]] =
      l match {
        case Cons(x,t) => f(x) match {
          case Right(x) => loop(t, acc.map(x :: _))
          case Left(x) => Left(x)
        }
        case _ => acc
      }
    loop(l, Right(Nil)).map(_.reverse)
  }
  def sequence[E,A](l: List[Either[E,A]]): Either[E,List[A]] =
    traverse(l)(x => x)

}