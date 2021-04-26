package hackerrank.medium.search

import scala.collection.immutable.TreeSet
import better.files._

object MinimumLoss {

  //https://www.hackerrank.com/challenges/minimum-loss/problem
  def minimumLoss(price: Array[Long]): Long = {
    val set = TreeSet.from(price)
    val ans = price.foldLeft((Long.MaxValue, set)) { case ((min, s), i) =>
      val ns   = s - i
      val diff = ns.maxBefore(i).map(i - _).getOrElse(min)
      (Math.min(diff, min), ns)
    }
    ans._1
  }

  val arr = file"/Users/chenh/Downloads/input03.txt".lines.flatMap(_.split(" ").map(_.toLong)).toArray
  println(minimumLoss(arr))
}
