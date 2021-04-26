package hackerrank.medium.graph

import better.files._

import scala.collection.mutable

object MinimumPenaltyPath {

  //https://www.hackerrank.com/challenges/beautiful-path/problem
  def bfs(
    graph: Map[Int, Map[Int, Set[Long]]],
    end: Int,
    visited: Set[(Int, Long)],
    pending: mutable.Queue[(Int, Int, Long)]): Long =
    LazyList
      .unfold(pending.isEmpty)(e => if (e) None else Some((pending.dequeue(), pending.isEmpty)))
      .dropWhile(x => visited((x._2, x._3)))
      .headOption match {
      case None =>
        visited.filter(_._1 == end).map(_._2).minOption.getOrElse(-1)
      case Some((of, ot, oc)) =>
        graph(ot).removed(of).foreach { case (t, s) =>
          s.foreach { x =>
            val n = oc | x
            if (!visited(t, n)) pending.enqueue((ot, t, n))
          }
        }
        bfs(graph, end, visited + ((ot, oc)), pending)
    }

  def beautifulPath(edges: Array[Array[Int]], A: Int, B: Int): Int = {
    val graph = edges.flatMap { case Array(f, t, w) => Array((f, t, w), (t, f, w)) }.groupBy(_._1).map { case (f, es) =>
      f -> es.groupBy(_._2).flatMap { case (t, es) => if (f == t) None else Some((t -> es.map(_._3.toLong).toSet)) }
    }
    graph.get(A) match {
      case None => -1
      case Some(m) =>
        val first = m.toList.flatMap { case (t, s) => s.map(x => (A, t, x)) }
        bfs(graph, B, Set.empty, mutable.Queue.from(first)).toInt
    }
  }

  val arr = Array(Array(1, 2, 1), Array(1, 2, 1000), Array(2, 3, 3), Array(1, 3, 100))
  println(beautifulPath(arr, 1, 3))
  val r1 = file"/Users/chenh/Downloads/input01.txt".lines.map(_.split(" ").map(_.toInt)).toArray
  println(beautifulPath(r1, 34, 29))

//  val r5 = file"/Users/chenh/Downloads/input05.txt".lines.map(_.split(" ").map(_.toInt)).toArray
//  println(beautifulPath(r5, 887, 858))
}
