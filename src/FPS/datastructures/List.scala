package FPS.datastructures

/**
  * Created by luca.catallo.293@gmail.com on 19/07/2016.
  */

sealed trait List[+A] {
  def ==[B >: A](l: List[B]): Boolean = List.equals(this, l)
  def ::[B >: A](b: B): List[B] = Cons(b, this)
  def append[B >: A](l: List[B]): List[B] = List.append(this, l)
  def drop(n: Int) = List.drop(this, n)
  def dropWhile(p: A => Boolean) = List.dropWhile(this)(p)
  def foldLeft[B](z: B)(f: (B, A) => B) = List.foldLeft(this, z)(f)
  def foldRight[B](z: B)(f: (A, B) => B) = List.foldRightViaFoldLeft(this, z)(f)
  def length: Int = List.lengthViaFoldLeft(this)
  def map[B](f: A => B): List[B] = List.map(this)(f)
  def reverse = List.reverse(this)
  def zipWith[B,C](bs: List[B])(f: (A,B) => C): List[C] = List.zipWith(this, bs)(f)

  override def toString: String = this.foldLeft("List(")((s, el) => s + el + ",").dropRight(1) + ")"
}
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(h,t) => h + sum(t)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Cons(0.0, _) => 0
    case Cons(h,t) => h * product(ds)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // 3.10
  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B = {
    @annotation.tailrec
    def loop(as: List[A], acc: B): B = as match {
      case Cons(h,t) => loop(t, f(acc,h))
      case _ => acc
    }
    loop(as, z)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B =
    as match {
      case Cons(h,t) => f(h, foldRight(t, z)(f))
      case _ => z
    }

  // 3.2
  def tail[A](as: List[A]): List[A] = as match {
    case Cons(h,t) => t
    case _ => Nil
  }

  // 3.3
  def setHead[A](as: List[A], a: A): List[A] = as match {
    case Cons(h, t) => Cons(a, t)
    case _ => Nil
  }

  // 3.4
  @annotation.tailrec
  def drop[A](as: List[A], n: Int): List[A] = (as, n) match {
    case (_, 0) => as
    case (Cons(h, t), _) => drop(t, n-1)
    case _ => Nil
  }

  // 3.5
  @annotation.tailrec
  def dropWhile[A](as: List[A])(p: A => Boolean): List[A] = as match {
    case Cons(h, t) => if (p(h)) dropWhile(t)(p) else as
    case Nil => Nil
  }

  // 3.6
  def init[A](as: List[A]): List[A] = {
    @annotation.tailrec
    def loop(as: List[A], acc: List[A]): List[A] = as match {
      case Cons(h,Nil) => acc
      case Cons(h,t) => loop(t, Cons(h, acc))
      case _ => acc
    }
    loop(as, Nil).reverse
  }

  // 3.9
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_,c) => c+1)

  // 3.11
  def sumViaFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)
  def productViaFoldLeft(ds: List[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)
  def lengthViaFoldLeft[A](as: List[A]): Int =
    foldLeft(as, 0)((c,_) => c+1)

  // 3.12
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil:List[A])((b,a) => Cons(a,b))

  // 3.13
  def foldLeftViaFoldRight[A,B](as: List[A], z: B)(f: (B,A) => B): B =
    foldRight(as, (b: B) => b)((a,g) => (b: B) => g(f(b,a)))(z)
  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(as.reverse, z)((b,a) => f(a,b))

  // 3.14
  def append[A](a1: List[A], a2: List[A]): List[A] =
    foldRightViaFoldLeft(a1, a2)(Cons(_,_))

  // 3.15
  def flatten[A](ls: List[List[A]]): List[A] =
    foldRightViaFoldLeft(ls, Nil:List[A])(append)

  // 3.16
  def plusOne(ns: List[Int]): List[Int] =
    foldRightViaFoldLeft(ns, Nil:List[Int]){
      (n,acc) => Cons(n+1, acc)
    }

  // 3.17
  def doubleToString(ds: List[Double]): List[String] =
    foldRightViaFoldLeft(ds, Nil:List[String]){
      (d, acc) => Cons(d.toString, acc)
    }

  // 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft(as, Nil:List[B]){
      (a, acc) => Cons(f(a), acc)
    }

  // 3.19
  def filter[A](as: List[A])(p: A => Boolean): List[A] =
    foldRightViaFoldLeft(as, Nil:List[A]){
      (a,acc) => if (p(a)) Cons(a,acc) else acc
    }

  // 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    flatten(map(as)(f))

  // 3.21
  def filterViaFlatMap[A](as: List[A])(p: A => Boolean): List[A] =
    flatMap(as)(a => if (p(a)) List(a) else Nil)

  // 3.22
  def sumCorresponding(l1: List[Int], l2: List[Int]): List[Int] = {
    @annotation.tailrec
    def loop(l1: List[Int], l2: List[Int], acc: List[Int]): List[Int] = (l1, l2) match {
      case (Cons(x1,t1),Cons(x2,t2)) => loop(t1, t2, Cons(x1+x2, acc))
      case _ => acc
    }
    loop(l1, l2, Nil)
  }

  // 3.23
  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] = {
    @annotation.tailrec
    def loop(as: List[A], bs: List[B], acc: List[C]): List[C] = (as, bs) match {
      case (Cons(a,ta),Cons(b,tb)) => loop(ta, tb, Cons(f(a,b), acc))
      case _ => acc
    }
    loop(as, bs, Nil)
  }

  @annotation.tailrec
  def startWith[A](as: List[A], ps: List[A]): Boolean = (as, ps) match {
    case (_, Nil) => true
    case (Cons(a, ta), Cons(p, tp)) => p != a || startWith(ta, tp)
    case _ => false
  }

  // 3.24
  @annotation.tailrec
  def hasSubsequence[A](as: List[A], ps: List[A]): Boolean = as match {
    case Cons(x,t) => startWith(as, ps) || hasSubsequence(t, ps)
    case _ => false
  }

  @annotation.tailrec
  def equals[A](l1: List[A], l2: List[A]): Boolean = (l1, l2) match {
    case (Nil, Nil) => true
    case (Cons(x1,t1), Cons(x2,t2)) => x1 == x2 && equals(t1, t2)
    case _ => false
  }

}