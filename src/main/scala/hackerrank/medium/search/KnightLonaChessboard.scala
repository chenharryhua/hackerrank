package hackerrank.medium.search

import scala.collection.mutable

//https://www.hackerrank.com/challenges/knightl-on-chessboard/problem
object KnightLonaChessboard extends App {

  final case class KnightL(a: Int, b: Int, n: Int) {
    def bingo(r: Int, c: Int): Boolean = n == r && n == c

    def move(r: Int, c: Int): List[(Int, Int)] =
      List(
        (r + a, c + b),
        (r - a, c - b),
        (r + a, c - b),
        (r - a, c + b),
        (r + b, c + a),
        (r - b, c - a),
        (r + b, c - a),
        (r - b, c + a)).filter { case (r, c) => r >= 0 && r <= n && c >= 0 && c <= n }.distinct
  }

  def bfs(knight: KnightL, queue: mutable.Queue[(Int, Int, Int)], visited: Set[(Int, Int)]): Int =
    LazyList
      .unfold(queue.isEmpty)(s => if (s) None else Some((queue.dequeue(), queue.isEmpty)))
      .dropWhile(x => visited(x._1, x._2))
      .headOption match {
      case None => -1
      case Some((r, c, ms)) =>
        val next = knight.move(r, c)
        next.find(x => knight.bingo(x._1, x._2)) match {
          case Some((_, _)) => ms + 1
          case None =>
            queue.enqueueAll(next.map { case (a, b) => (a, b, ms + 1) })
            bfs(knight, queue, visited + (r -> c))
        }
    }

  def knightlOnAChessboard(n: Int): Array[Array[Int]] = {
    val ans = (1 until n)
      .map(a => (1 until n).map(b => bfs(KnightL(a, b, n - 1), mutable.Queue((0, 0, 0)), Set.empty)).toArray)
      .toArray
    ans
  }

  val ans = knightlOnAChessboard(5)
  ans.foreach(x => println(x.toList))
}
