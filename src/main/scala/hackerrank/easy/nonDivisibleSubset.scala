package hackerrank.easy

import better.files._
import File._

object nonDivisibleSubset {

  def nonDivisibleSubset(k: Int, s: Array[Int]): Int = {
    val map = s.groupBy(_ % k).map { case (i, as) => (i, as.length) }
    if (k == 1) 1
    else {
      val h = if (map.get(0).exists(_ > 0)) 1 else 0
      val m = if (k % 2 == 0 && map.get(k / 2).exists(_ > 0)) 1 else 0

      val b = (1 until (k + 1) / 2)
        .map(x => (x, k - x))
        .toList
        .map { case (a, b) =>
          println(a, b)
          Math.max(map.getOrElse(a, 0), map.getOrElse(b, 0))
        }
        .sum
      h + m + b
    }

  }

  val f: File = file"./data.txt"
  val arr     = f.contentAsString.split(" ").map(_.toInt)

  println(arr.length)
  println(nonDivisibleSubset(100, arr))
}
