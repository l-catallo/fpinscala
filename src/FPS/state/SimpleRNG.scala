package FPS.state

/**
  * Created by luca.catallo.293@gmail.com on 11/08/2016.
  */


case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, SimpleRNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >> 16).toInt
    (n, nextRNG)
  }
}