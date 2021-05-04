package hackerrank.medium.recursion

// https://www.hackerrank.com/challenges/the-power-sum/problem
object PowerSum {

  def powerSum(X: Int, N: Int): Int = {
    def solve(x: Int, n: Int, i: Int): Int = {
      val tmp = x - Math.pow(i, n).toInt
      if (tmp == 0) 1
      else if (tmp > 0) {
        (i + 1 to tmp).foldLeft(0) { case (s, j) => s + solve(tmp, n, j) }
      } else 0

    }
    solve(X, N, 0)
  }
}
