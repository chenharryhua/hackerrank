package hackerrank.medium.dp

import scala.collection.immutable.TreeSet

// https://www.hackerrank.com/challenges/unbounded-knapsack/problem

object Knapsack {

  def unboundedKnapsack(k: Int, arr: Array[Int]): Int = {
    val set = arr.flatMap(x => LazyList.from(1).map(y => x * y).takeWhile(_ <= k))
    val ts  = TreeSet.from(set)
    ts.flatMap { x =>
      val diff = k - x + 1
      ts.maxBefore(diff).map(_ + x).orElse(Some(x))
    }.maxOption.getOrElse(0)
  }
  println(unboundedKnapsack(13, Array(3, 7, 9, 11)))

}
