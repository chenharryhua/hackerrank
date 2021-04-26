package hackerrank.medium.graph

object Clique {

  // https://www.hackerrank.com/challenges/clique/problem
  def turan(n: Int, r: Int): Double = {
    val mod: Double = n % r
    val div: Double = n / (r * 1.0)
    val f: Double   = Math.floor(div)
    val c: Double   = Math.ceil(div)
    0.5d * (n * n * 1.0d - mod * c * c - (r - mod) * f * f)
  }

  def clique(n: Int, m: Int): Int = {
    def go(a: Int): Int = if (turan(n, a).toInt < m) a + 1 else go(a - 1)
    go(n - 1)
  }

  println(clique(9831, 48319360))
}
