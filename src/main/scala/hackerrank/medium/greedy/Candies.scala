package hackerrank.medium.greedy

import better.files._

object Candies {

  // https://www.hackerrank.com/challenges/candies/problem?h_r=internal-search
  def candies(n: Int, arr: Array[Int]): Long = {
    val len = arr.length
    if (len == 0) 0
    else if (len == 1) 1
    else if (len == 2) 3
    else {
      val s = arr.sliding(2).map(arr => (arr(0), arr(1))).toList
      val l = s.scanLeft(1L) { case (s, (l, r)) => if (l < r) s + 1 else 1 }
      val r = s.scanRight(1L) { case ((l, r), s) => if (l > r) s + 1 else 1 }
      l.zip(r).map { case (a, b) => Math.max(a, b) }.sum
    }
  }
  val rr = file"/Users/chenh/Downloads/input11.txt".lines.map(_.toInt).tail.toArray
  println(candies(1, rr)) // Array(1, 2, 3, 2, 1)))
}
