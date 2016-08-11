package FPS.state

/**
  * Created by luca.catallo.293@gmail.com on 11/08/2016.
  */

import FPS.datastructures.List


case class State[S,+A](run: S => (A,S)) {

  def apply(s: S): (A,S) = run(s)

  def flatMap[B](f: A => State[S,B]): State[S,B] =
    State {
      s => {
        val (aa, s1) = this(s)
        val (bb, s2) = f(aa)(s1)
        (bb, s2)
      }
    }

  def map[B](f: A => B): State[S,B] =
    State {
      s => {
        val (a, s1) = this(s)
        (f(a), s1)
      }
    }

  def map2[B,C](b: State[S,B])(f: (A,B) => C): State[S,C] =
    this.flatMap(aa => b.map(bb => f(aa,bb)))

}

object State {

  def unit[S,A](a: A): State[S,A] =
    State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    State{
      (s: S) => sas.foldLeft((List():List[A], s)){
        (acc, el) => {
          val (a, s2) = el(acc._2)
          (a :: acc._1, s2)
        }
      }
    }.map(_.reverse)

  def get[S]: State[S,S] =
    State(s => (s, s))

  def set[S](s: S): State[S,Unit] =
    State(_ => ((), s))

  def modify[S](f: S => S): State[S,Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

}

