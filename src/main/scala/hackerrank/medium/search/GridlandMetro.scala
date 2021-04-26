package hackerrank.medium.search

import better.files._

object GridlandMetro {

  // https://www.hackerrank.com/challenges/gridland-metro/problem
  def gridlandMetro(n: Int, m: Int, k: Int, track: Array[Array[Long]]): Long = {
    val total = n * m.toLong
    val exist = track
      .groupBy(_(0))
      .map { case (_, arr) =>
        arr.map { case Array(_, f, t) => (f, t) }
          .sortBy(_._1)
          .foldLeft((0L, 0L)) { case ((s, last), (f, t)) =>
            if (t <= last) (s, last)
            else if (f > last) (s + t - f + 1, t)
            else (s + t - last, t)
          }
          ._1
      }
      .sum
    total - exist
  }

  val arr = file"/Users/chenh/Downloads/input06.txt".lines.map(_.split(" ").map(_.toLong)).toArray
  println(gridlandMetro(402159386, 855281517, 951, arr))
}
