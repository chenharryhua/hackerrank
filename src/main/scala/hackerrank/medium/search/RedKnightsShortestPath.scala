package hackerrank.medium.search

import scala.collection.mutable

//https://www.hackerrank.com/challenges/red-knights-shortest-path/problem
object RedKnightsShortestPath {

  object Move extends Enumeration {
    type Move = Value
    val UL, UR, R, LR, LL, L = Value
  }

  def move(m: Move.Move, n: Int, r: Int, c: Int): Option[(Int, Int)] = m match {
    case Move.UL => if (r - 2 >= 0 && c - 1 >= 0) Some(r - 2, c - 1) else None
    case Move.UR => if (r - 2 >= 0 && c + 1 <= n) Some(r - 2, c + 1) else None
    case Move.R  => if (c + 2 <= n) Some(r, c + 2) else None
    case Move.LR => if (r + 2 <= n && c + 1 <= n) Some(r + 2, c + 1) else None
    case Move.LL => if (r + 2 <= n && c - 1 >= 0) Some(r + 2, c - 1) else None
    case Move.L  => if (c - 2 >= 0) Some(r, c - 2) else None
  }

  val moves: List[Move.Move] = List(Move.UL, Move.UR, Move.R, Move.L, Move.LL, Move.LR)

  def bfs(
    n: Int,
    i_end: Int,
    j_end: Int,
    queue: mutable.Queue[(Int, Int, List[Move.Move])],
    visited: Set[(Int, Int)]): List[Move.Move] =
    LazyList
      .unfold(queue.isEmpty)(s => if (s) None else Some((queue.dequeue(), queue.isEmpty)))
      .dropWhile(x => visited(x._1, x._2))
      .headOption match {
      case None => List()
      case Some((r, c, ms)) =>
        val next = moves
          .flatMap(op =>
            (move(op, n, r, c).map { case (s, e) =>
              if (s == i_end && e == j_end) (s, e, op, true) else (s, e, op, false)
            }))
          .sortBy(_._3)
        next.find(_._4) match {
          case Some((_, _, m, _)) => m :: ms
          case None =>
            next.foreach { case (r, c, m, _) => queue.enqueue((r, c, m :: ms)) }
            bfs(n, i_end, j_end, queue, visited + (r -> c))
        }
    }

  def printShortestPath(n: Int, i_start: Int, j_start: Int, i_end: Int, j_end: Int): Unit = {
    val ans = bfs(n - 1, i_end, j_end, mutable.Queue((i_start, j_start, List.empty)), Set.empty)
    if (ans.isEmpty) println("Impossible")
    else {
      println(ans.length)
      println(ans.reverse.mkString(" "))
    }
  }

  printShortestPath(7, 6, 6, 0, 1)
}
