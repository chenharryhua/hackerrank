package hackerrank.medium.dp

// https://www.hackerrank.com/challenges/coin-change/problem
object TheCoinChangeProblem {

  def go(n: Int, denom: Int, denoms: List[Int], cache: Map[(Int, Int), Long]): (Long, Map[(Int, Int), Long]) =
    denoms match {
      case Nil => if (n % denom == 0) (1L, cache + ((n, denom) -> 1L)) else (0L, cache + ((n, denom) -> 0L))
      case head :: rest =>
        LazyList.from(0).map(_ * denom).takeWhile(_ <= n).foldLeft((0L, cache)) { case ((s, m), i) =>
          m.get((n - i, head)) match {
            case Some(a) => (s + a, m)
            case None =>
              val (r, nm) = go(n - i, head, rest, m)
              (s + r, nm + ((n - i, head) -> r))
          }
        }
    }

  def getWays(n: Int, c: Array[Int]): Long = {
    val denoms = c.toList.sorted.reverse
    go(n, denoms.head, denoms.tail, Map.empty)._1
  }

  println(getWays(10, Array(6, 5, 3, 2)))
  println(getWays(166, Array(5, 37, 8, 39, 33, 17, 22, 32, 13, 7, 10, 35, 40, 2, 43, 49, 46, 19, 41, 1, 12, 11, 28)))
}

object NonDP {

  def go(n: Int, denom: Int, denoms: List[Int]): Long =
    denoms match {
      case Nil => if (n % denom == 0) 1L else 0L
      case head :: rest =>
        LazyList.from(0).map(_ * denom).takeWhile(_ <= n).map(x => go(n - x, head, rest)).sum
    }

  def getWays(n: Int, c: Array[Int]): Long = {
    val denoms = c.toList.sorted.reverse
    go(n, denoms.head, denoms.tail)
  }
}
