package hackerrank.medium.dp

import better.files._

// https://www.hackerrank.com/challenges/xor-and-sum/problem

// A ^ B = (A + B) - 2 * (A & B)

object XorandSum extends App {
  val M: Long = 1000000007L
  val S: Int  = 314159 + 1

  def sum(idx: Int, a: Long, b: Long, s: Long): Long =
    if (idx == S) s else sum(idx + 1, a, (b * 2) % M, (a + b + s) % M)

  def and(n: Int, c: Int, a: BigInt, b: BigInt, s: BigInt): Long =
    if (n == c) ((s * 2) % M).toLong else and(n, c + 1, a, b * 2, s + (a & b))

  def xorAndSum(a: String, b: String): Long = {
    val aa  = BigInt(a, 2)
    val bb  = BigInt(b, 2)
    val r1  = and(a.length, 0, aa, bb, 0)
    val r2  = sum(0, (aa % M).toLong, (bb % M).toLong, 0)
    val ans = r2 - r1
    if (ans > 0) ans else M + ans
  }
  val arr = file"/Users/chenh/Downloads/input07.txt".lines.toList
  println(xorAndSum(arr(0), arr(1)))
}

object XorandSumBF {

  def xorAndSum(a: String, b: String): Long = {
    val aa = BigInt(a, 2)
    val bb = BigInt(b, 2)
    (0 to 314159).foldLeft(0L) { case (s, i) =>
      ((s + (aa ^ (bb << i))) % 1000000007L).toLong
    }
  }
}
