package hackerrank.medium.greedy

object MaxMin {
  //https://www.hackerrank.com/challenges/angry-children/problem

  def maxMin(k: Int, arr: Array[Int]): Int = {
    val len    = arr.length
    val sorted = arr.sorted
    val m      = (0 until len - k).map(x => sorted(x + k - 1) - sorted(x)).min
    val r      = sorted.takeRight(k)
    Math.min(m, r(k - 1) - r(0))
  }

  def pairs(k: Int, arr: Array[Int]): Int = {
    val set = arr.toSet
    arr.foldLeft(0) { case (s, d) => if (set.contains(d + k)) s + 1 else s }
  }

  def gridChallenge(grid: Array[String]): String = {
    val len  = grid.length
    val len2 = grid(0).length
    def check(grid: Array[String]) = {
      val res = for {
        j <- (0 until len2)
        i <- (1 until len)
      } yield grid(i - 1)(j) <= grid(i)(j)
      res.forall(identity)
    }
    grid.sliding(2).map { case Array(a, b) => a <= b }

    if (check(grid)) "YES"
    else if (check(grid.sorted)) "YES"
    else "NO"

  }
}
