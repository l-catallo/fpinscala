package FPS.state

/**
  * Created by luca.catallo.293@gmail.com on 11/08/2016.
  */

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  type State[S,+A] = S => (A,S)

  type Rand[+A] = State[RNG, A]

  def int(rng: RNG): (Int, RNG) = rng.nextInt

  // 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) =
  rng.nextInt match {
    case (Int.MinValue, nextRNG) => nonNegativeInt(nextRNG)
    case (r, nextRNG) => (Math.abs(r), nextRNG)
  }

  // 6.2
  def double(rng: RNG): (Double, RNG) =
  nonNegativeInt(rng) match {
    case (r, nextRNG) => (-r.toDouble / Int.MinValue, nextRNG)
  }

  // 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (ri, rng1) = rng.nextInt
    val (rd, rng2) = double(rng1)
    ((ri, rd), rng2)
  }
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (rd, rng1) = double(rng)
    val (ri, rng2) = rng1.nextInt
    ((rd, ri), rng2)
  }
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (r1, rng1) = double(rng)
    val (r2, rng2) = double(rng1)
    val (r3, rng3) = double(rng2)
    ((r1, r2, r3), rng3)
  }

  // 6.4
  def ints(n: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(n: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) =
      if (n>0) {
        val (r, nextRNG) = rng.nextInt
        loop(n-1, nextRNG, r :: acc)
      } else
        (acc, rng)
    loop(n, rng, Nil)
  }

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // 6.5
  def doubleMap(rng: RNG): (Double, RNG) =
  map(nonNegativeInt)(r => -r.toDouble / Int.MinValue)(rng)

  // 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
  rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a,b), rng2)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra,rb)((_, _))

  // 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    def loop(fs: List[Rand[A]], acc:List[A], rng: RNG): (List[A], RNG) =
      fs match {
        case h :: t => {
          val (a, nextRNG) = h(rng)
          loop(t, a :: acc, nextRNG)
        }
        case _ => (acc, rng)
      }
    rng => loop(fs, Nil, rng)
  }
  def intsSequence(n: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(n)(int _))(rng)

  // 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng1) = f(rng)
    val (b, rng2) = g(a)(rng1)
    (b, rng2)
  }
  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt){
      i =>
        val mod = i % n
        if (i + (n-1) - mod >= 0) unit(mod)
        else nonNegativeLessThan(n)
    }

  // 6.9
  def mapFl[A,B](s: Rand[A])(f: A => B): Rand[B] =
  flatMap(s)(a => unit(f(a)))
  def map2Fl[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    flatMap(ra)( a => mapFl(rb)(b => f(a,b)) )

}
