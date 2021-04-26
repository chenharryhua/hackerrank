package hackerrank.medium.graph

import scala.collection.immutable.TreeMap
import scala.collection.mutable

object RustMurderer {

  // https://www.hackerrank.com/challenges/rust-murderer/problem
  def dijkstra(
    graph: Array[Set[Int]],
    es: Set[Int],
    pending: mutable.PriorityQueue[(Int, Long)],
    visited: Set[Int],
    table: TreeMap[Int, Long]): TreeMap[Int, Long] =
    LazyList
      .unfold(pending.isEmpty)(e => if (e) None else Some((pending.dequeue(), pending.isEmpty)))
      .dropWhile(x => visited(x._1))
      .headOption match {
      case None => table
      case Some((n, w)) =>
        val tb = graph(n).foldLeft(table) { case (tb, i) =>
          val cw = table.getOrElse(i, Long.MaxValue)
          val nw = w + 1
          if (nw < cw && !visited(i)) {
            pending.enqueue((i, nw))
            tb.updated(i, nw)
          } else tb
        }
        dijkstra(graph, es, pending, visited + n, tb)
    }

  def rustMurdered(n: Int, s: Int, roads: Array[Set[Int]], es: Set[Int]): Array[Int] =
    dijkstra(roads, es, mutable.PriorityQueue((s, 0L)), Set.empty, TreeMap(s -> 0L)).removed(s).map(_._2.toInt).toArray

  val n = 0
  val m = 0

  val edges = (0 until m).flatMap { _ =>
    val r = io.StdIn.readLine.split(" ").map(_.toInt)
    Array(((r(0), r(1))), (r(1), r(0)))
  }
  val nodes: Set[Int]            = Set.from((1 to n))
  val cities: Map[Int, Set[Int]] = edges.groupMap(_._1)(_._2).map { case (f -> e) => (f -> (nodes.removedAll(e) - f)) }

  val arr   = Array.fill(n + 1)(nodes)
  val roads = arr.zipWithIndex.map { case (s, i) => s -- cities.getOrElse(i, Set.empty) - i }

}
