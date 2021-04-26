package hackerrank.medium.search

import scala.collection.immutable.TreeSet
import better.files._

object HackerlandRadioTransmitters {

//https://www.hackerrank.com/challenges/hackerland-radio-transmitters/problem
  def hackerlandRadioTransmitters(x: Array[Int], k: Int): Int = {

    val ts = TreeSet.from(x)
    LazyList
      .unfold(ts.head) { idx =>
        val first  = ts.range(idx, idx + k + 1).last
        val second = ts.range(first, first + k + 1).last
        ts.minAfter(second + 1).map((1, _))
      }
      .sum + 1
  }
  val arr = file"/Users/chenh/Downloads/input06.txt".lines.flatMap(_.split(" ").map(_.toInt)).toArray
  val ans = hackerlandRadioTransmitters(arr, 80)
  println(ans)
}
