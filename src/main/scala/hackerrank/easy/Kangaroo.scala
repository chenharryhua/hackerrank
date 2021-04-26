package hackerrank.easy

import java.time.LocalDateTime

object Kangaroo {

  def kangaroo(x1: Int, v1: Int, x2: Int, v2: Int): String = {
    val s1 = LazyList.from(x1, v1)
    val s2 = LazyList.from(x2, v2)

    def result(ll: LazyList[(Int, Int)]): Boolean = {
      println(ll.head)
      ll match {
        case (a, b) #:: rest =>
          if (a == b) true else if (a > b) result(rest) else false
      }
    }

    val r =
      if (x2 >= x1 && v2 >= v1) false
      else if (x2 <= x1 && v2 <= v1) false
      else { if (x2 > x1) result(s2.zip(s1)) else result(s1.zip(s2)) }

    if (r) "YES" else "NO"

  }

  def jumpingOnClouds(c: Array[Int]): Int = {
    val b = c
      .foldLeft((0, List.empty[Int])) { case ((s, l), i) =>
        println(l, s)
        if (i == 1 && l.length == 1) (s + 1, List.empty)
        else if (i == 1) (s + 1 + (l.length) / 2, List.empty)
        else (s, i :: l)
      }
      ._1
    val t = c.reverse.takeWhile(_ != 1).length / 2
    b + t
  }

}
