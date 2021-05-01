package hackerrank.medium.bit

import scala.collection.immutable.TreeSet

object Countergame {

  def counterGame(n: Long): String = {
    val ts = TreeSet.from((0 until 64).map(i => 1L << i))
    def go(n: Long, c: Int): Int =
      if (n == 1) c else if (ts.contains(n)) go(n / 2, c + 1) else go(n - ts.maxBefore(n).get, c + 1)
    val ans = go(n, 0)
    if (ans % 2 == 0) "Richard" else "Louise"
  }
}
