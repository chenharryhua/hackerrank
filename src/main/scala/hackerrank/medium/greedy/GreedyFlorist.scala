package hackerrank.medium.greedy

object GreedyFlorist {

  // https://www.hackerrank.com/challenges/greedy-florist/problem
  def getMinimumCost(k: Int, c: Array[Int]): Int =
    c.toList.sorted.reverse.grouped(k).zipWithIndex.map { case (l, n) => l.map(_ * (1 + n)).sum }.sum

}
