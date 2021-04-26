package hackerrank.medium.graph

import scala.collection.mutable

// https://www.hackerrank.com/challenges/the-quickest-way-up/problem
object SnakesandLadders {

  def bfs(
    up: Map[Int, Int],
    down: Map[Int, Int],
    die: List[Int],
    visited: Array[Boolean],
    pending: mutable.Queue[(Int, Long)]): Long =
    LazyList
      .unfold(pending.isEmpty)(s => if (s) None else Some((pending.dequeue(), pending.isEmpty)))
      .dropWhile(x => visited(x._1))
      .headOption match {
      case None => -1
      case Some((node, count)) =>
        if (node == 100) count
        else {
          val next = die
            .map(_ + node)
            .map(x => up.getOrElse(x, x))
            .map(x => down.getOrElse(x, x))
            .filter(_ <= 100)
            .map(x => (x, count + 1))
          val p = pending.appendAll(next)
          visited(node) = true
          bfs(up, down, die, visited, p)
        }
    }

  def quickestWayUp(ladders: Array[Array[Int]], snakes: Array[Array[Int]]): Int = {
    val up: Map[Int, Int]   = ladders.foldLeft(Map.empty[Int, Int]) { case (m, Array(f, t)) => m + (f -> t) }
    val down: Map[Int, Int] = snakes.foldLeft(Map.empty[Int, Int]) { case (m, Array(f, t)) => m + (f -> t) }
    val visited             = Array.fill(101)(false)
    val die                 = List(1, 2, 3, 4, 5, 6)
    bfs(up, down, die, Array.fill(101)(false), mutable.Queue((1, 0))).toInt
  }

  val snakes =
    Array(Array(99, 10), Array(97, 20), Array(98, 30), Array(96, 40), Array(95, 50), Array(94, 60), Array(93, 70))
  val ladders = Array(Array(3, 90))

  println(quickestWayUp(ladders, snakes))

}
