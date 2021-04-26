package hackerrank.easy

import scala.collection.immutable.TreeSet
import scala.collection.mutable.ArrayBuffer

object ForFun {

  def palindromeIndex(s: String): Int = {
    def check(s: String): Boolean = {
      val len = s.length
      (0 until len / 2).forall(x => s(x) == s(len - 1 - x))
    }

    val len    = s.length
    val arr    = s.toCharArray.zipWithIndex
    val (h, t) = arr.splitAt(len / 2)
    h.zip(t.reverse).dropWhile { case (a, b) => a._1 == b._1 }.headOption match {
      case None => -1
      case Some(((c1, i1), (c2, i2))) =>
        val tmp = ArrayBuffer.from(arr)
        tmp.remove(i1)
        if (check(tmp.map(_._1).mkString)) i1
        else {
          val tmp = ArrayBuffer.from(arr)
          tmp.remove(i2)
          if (check(tmp.map(_._1).mkString)) i2 else -1
        }
    }
  }

  def nimbleGame(s: Array[Int]): String = {
    val res = s.zipWithIndex.tail.foldLeft(0) { case (s, (d, idx)) => if (d % 2 == 1) s ^ idx else s }
    if (res > 0) "Second" else "First"

    s.tail.zipWithIndex.foldLeft(0) { case (s, i) => if (i._2 % 2 == 0) s ^ i._1 else s } match {
      case 0 => "Second"
      case _ => "First"
    }
  }

  def maximumPerimeterTriangle(sticks: Array[Int]): Array[Int] = {
    val (a, b, c, _) = sticks.toList
      .sliding(3)
      .flatMap { case List(a, b, c) => if (a + b > c) Some((a, b, c, a + b + c)) else None }
      .foldLeft((0, 0, 0, 0)) { case (s, (a, b, c, d)) =>
        if (d > s._4) (a, b, c, d)
        else if (c > s._3) (a, b, c, d)
        else if (a > s._1) (a, b, c, d)
        else s
      }
    Array(a, b, c)
  }

  def toys(w: Array[Int]): Int = {
    def reduce(ts: TreeSet[Int], count: Int): Int =
      if (ts.isEmpty) count
      else {
        val min = ts.min
        reduce(ts.removedAll(min to min + 4), count + 1)
      }

    reduce(TreeSet.from(w), 0)

  }
}
