package hackerrank.hard

import scala.annotation.tailrec

// https://www.hackerrank.com/challenges/dijkstrashortreach/problem
object DijkstraShortestReach {

  @tailrec
  def dijkstra(graph: Map[Int, Map[Int, Int]], curr: Int, visited: Set[Int], table: Map[Int, Int]): Map[Int, Int] = {
    val nextTable = graph.getOrElse(curr, Map.empty).foldLeft(table) { case (s, (node, weight)) =>
      s + (node -> Math.min(weight + table(curr), table.getOrElse(node, Int.MaxValue)))
    }
    val explored = visited + curr
    nextTable.removedAll(explored).minByOption(_._2) match {
      case None                => table
      case Some((nextNode, _)) => dijkstra(graph, nextNode, explored, nextTable)
    }
  }

  def shortestReach(n: Int, edges: Array[Array[Int]], s: Int): Array[Int] = {
    val graph = edges
      .groupBy(x => Set(x(0), x(1)))
      .map { case (s, ws) =>
        val min        = ws.minBy(_(2)).apply(2)
        val List(f, t) = s.toList
        (f, t, min)
      }
      .foldLeft(Map.empty[Int, Map[Int, Int]]) { case (m, (f, t, w)) =>
        m.updatedWith(f)(_.map(_ + (t -> w)).orElse(Some(Map(t -> w))))
          .updatedWith(t)(_.map(_ + (f -> w)).orElse(Some(Map(f -> w))))
      }

    val map = dijkstra(graph, s, Set(s), Map(s -> 0))
    (1 to n).filterNot(_ == s).map(x => map.getOrElse(x, -1)).toArray
  }

}
