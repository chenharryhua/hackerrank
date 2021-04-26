package hackerrank.easy

import better.files._

import scala.collection.immutable.TreeMap
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object beautifulPairs {

  def beautifulPairs(A: Array[Int], B: Array[Int]): Int = {
    val a = A.zipWithIndex.groupBy(_._1).map { case (a, b) => (a, b.length) }

    val (remain, beautiful, c) = B.foldLeft((a, 0, 0)) { case ((m, s, c), i) =>
      m.get(i) match {
        case None     => (m, s, 1)
        case Some(as) => if (as == 1) (m.removed(i), s + 1, c) else (m + (i -> (as - 1)), s + 1, c)
      }
    }

    if (B.length == beautiful) beautiful - 1 else beautiful + c
  }

  val a = file"/Users/chenh/Downloads/a.txt".contentAsString.split(" ").map(_.toInt)
  val b = file"/Users/chenh/Downloads/b.txt".contentAsString.split(" ").map(_.toInt)

  println(beautifulPairs(b, a))
}

object ttt {

  def balancedSums(arr: Array[Int]): String = {
    val sum = arr.sum
    val res = arr.foldLeft((0, false)) { case ((s, f), i) => if ((sum - i) / 2 == s) (s + i, true) else (s, false) }
    if (res._2) "YES" else "NO"

  }

  final class BMap(kv: Map[Int, Int], vk: Map[Int, Int]) {

    def swap(k: Int, v: Int): BMap = {
      val ov = kv(k)
      val ok = vk(v)
      new BMap(kv ++ Map(k -> v, ok -> ov), vk ++ Map(v -> k, ov -> ok))
    }
    def result: List[Int] = kv.toList.sortBy(_._1).map(_._2)

    override def toString: String = kv.toString()
  }

  def largestPermutation(k: Int, arr: Array[Int]): Array[Int] = {
    val m      = arr.zipWithIndex.toMap
    val len    = arr.length
    val sorted = arr.sorted.reverse
    def count(k: Int, idx: Int, m: Map[Int, Int]): Unit =
      if (k == 0 || idx >= len) ()
      else if (sorted(idx) > arr(idx)) {
        val i   = m(sorted(idx))
        val tmp = arr(i)
        arr(i) = arr(idx)
        arr(idx) = tmp
        count(k - 1, idx + 1, m + (arr(i) -> i) + (arr(idx) -> idx))
      } else count(k, idx + 1, m)
    count(k, 0, m)
    arr
  }

  def decentNumber(n: Int) {
    LazyList.from(n, -5).zipWithIndex.takeWhile(_._1 >= 0).find(_._1 % 3 == 0) match {
      case None         => println("-1")
      case Some((f, t)) => println("5" * f + "3" * (n - f))
    }
  }

  def gridChallenge(grid: Array[String]): String = {
    def check(s: Array[String]): Boolean = {
      val a    = s.map(_.toCharArray)
      val len  = s.length
      val len2 = s(0).length
      val m = for {
        j <- 0 until len2
        i <- 0 until len
      } yield a(i)(j)
      m.grouped(len).map(_.toList.sliding(2).forall { case List(a, b) => a < b }).toList.forall(identity)
    }
    if (check(grid.map(_.sorted))) "YES" else "NO"
  }

}
