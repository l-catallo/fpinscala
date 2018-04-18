package FPS.datastructures

/**
  * Created by luca.catallo.293@gmail.com on 09/08/2016.
  */

sealed trait Tree[+A] {
  def depth: Int = Tree.foldDepth(this)
  def fold[B](ac: A => B)(f: (B,B) => B): B = Tree.fold(this)(ac)(f)
  def map[B](f: A => B): Tree[B] = Tree.map(this)(f)
}
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // 3.26
  def maximum[A](t: Tree[A])(f: (A,A) => A): A = t match {
    case Leaf(x) => x
    case Branch(l,r) => f(maximum(l)(f), maximum(r)(f))
  }

  // 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l,r) => (depth(l) max depth(r)) + 1
  }

  // 3.28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // 3.29
  def fold[A,B](t: Tree[A])(ac: A => B)(f: (B,B) => B): B = t match {
    case Leaf(x) => ac(x)
    case Branch(l,r) => f(fold(l)(ac)(f), fold(r)(ac)(f))
  }
  def foldSize[A](t: Tree[A]): Int =
    fold(t)(x => 1)(_ + _ + 1)
  def foldMaximum[A](t: Tree[A])(f: (A,A) => A): A =
    fold(t)(x => x)(f(_,_))
  def foldDepth[A](t: Tree[A]): Int =
    fold(t)(x=>0)((x,y) => (x max y) + 1)
}
