package FPS.laziness

/**
  * Created by luca.catallo.293@gmail.com on 09/08/2016.
  */

import FPS.datastructures.{List, Nil}
import FPS.errorhandling.{Option, Some, None}


sealed trait Stream[+A] {
  def tailOption: Option[Stream[A]] =
    this match {
      case Cons(s, t) => Some(t())
      case _ => None
    }

  // 5.1
  def toList: List[A] = {
    @annotation.tailrec
    def loop(s: Stream[A], acc: List[A]): List[A] =
      s match {
        case Empty => acc
        case Cons(h,t) => loop(t(), h() :: acc)
      }
    loop(this, Nil).reverse
  }

  // 5.2
  def take(n: Int): Stream[A] =
  this match {
    case Empty => Empty
    case Cons(h,t) => if (n>0) Cons(h, () => t().take(n-1)) else Empty
  }
  def drop(n: Int): Stream[A] =
    this match {
      case Empty => Empty
      case Cons(h,t) => if (n<=0) Cons(h, t) else t().drop(n-1)
    }

  // 5.3
  def takeWhile(f: A => Boolean): Stream[A] =
  this match {
    case Empty => Empty
    case Cons(h,t) => if (f(h())) Cons(h, () => t().takeWhile(f)) else Empty
  }

  def exists(f: A => Boolean): Boolean =
    this match {
      case Cons(h,t) => f(h()) || t().exists(f)
      case _ => false
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  // 5.4
  def forAll(p: A => Boolean): Boolean =
  this.foldRight(true)((a, b) => p(a) && b)

  // 5.5
  def takeWhile2(p: A => Boolean): Stream[A] =
  this.foldRight(Empty:Stream[A])((a, b) => if (p(a)) Cons(()=>a, ()=>b) else b)

  // 5.7
  def map[B](f: A => B): Stream[B] =
  this.foldRight(Empty:Stream[B])((a,b) => Cons(()=>f(a), ()=>b))
  def filter(p: A => Boolean): Stream[A] =
    this.foldRight(Empty:Stream[A])((a,b) => if (p(a)) Cons(()=>a,()=>b.filter(p)) else b)
  def append[B >: A](x: => Stream[B]): Stream[B] =
    this.foldRight(x)((a,b) => Cons(()=>a, ()=>b))
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    this.foldRight(Empty:Stream[B])((a,b) => f(a).append(b))

  // 5.13
  def map2[B](f: A => B): Stream[B] =
  Stream.unfold(this){
    case Cons(h,t) => Some(f(h()), t())
    case _ => None
  }
  def take2(n: Int): Stream[A] =
    Stream.unfold((this,n))(x => x._1 match {
      case Cons(h,t) =>
        if (x._2>0) Some(h(), (t(), x._2-1))
        else None
      case _ => None
    })
  def takeWhile3(p: A => Boolean): Stream[A] =
    Stream.unfold(this){
      case Cons(h,t) => {
        val hh = h()
        if (p(hh)) Some(hh, t())
        else None
      }
      case _ => None
    }
  def zipWith[B,C](s: Stream[B])(f: (A,B) => C): Stream[C] =
    Stream.unfold((this,s)){
      case (Cons(h1,t1), Cons(h2,t2)) => Some(f(h1(),h2()), (t1(), t2()))
      case _ => None
    }
  def zip[B](s: Stream[B]): Stream[(A,B)] =
    this.zipWith(s)((_,_))
  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this,s)){
      case (Cons(h1,t1), Cons(h2,t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h,t), _) => Some(((Some(h()), None), (t(), Empty)))
      case (_, Cons(h,t)) => Some(((None, Some(h())), (Empty, t())))
      case _ => None
    }

  // 5.14
  def startsWith[B >: A](s: Stream[B]): Boolean =
  Stream.unfold(this.zipAll(s)){
    case Cons(h,t) =>
      h() match {
        case (Some(x), Some(y)) => Some(x==y, t())
        case (Some(x), _) => None
        case (_, _) => Some(false, t())
      }
    case _ => None
  }.forAll(identity)

  // 5.15
  def tails: Stream[Stream[A]] =
  Stream.unfold(this){
    case Cons(h,t) => Some(Cons(h,t), t())
    case _ => None
  }

  // 5.16
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
  this.foldRight(Cons(()=>z,()=>Stream.empty))((a,z) => {
    Cons(()=>f(a, z.h()), ()=>z)
  })

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Cons(()=>1, ()=>ones)

  // 5.8
  def constant[A](c: A): Stream[A] = cons(c, constant(c))

  // 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  // 5.10
  def fibs: Stream[Int] = {
    def loop(a: Int, b: Int): Stream[Int] = cons(a, loop(b, a+b))
    loop(0,1)
  }

  // 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a,zz)) => cons(a, unfold(zz)(f))
      case _ => Empty
    }
  }

  // 5.12
  def fibs2: Stream[Int] = unfold((0, 1))(a => Some(a._1, (a._2, a._1 + a._2)))
  def from2(n: Int): Stream[Int] = unfold(n)(a => Some(a, a+1))
  def constant2(n: Int): Stream[Int] = unfold(n)(a => Some(a, a))
  def ones2: Stream[Int] = unfold(1)(a => Some(a, a))

}
