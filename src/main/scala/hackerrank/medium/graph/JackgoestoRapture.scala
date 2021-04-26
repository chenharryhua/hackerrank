package hackerrank.medium.graph

import better.files._

import java.time._
import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.io.StdIn

// https://www.hackerrank.com/challenges/jack-goes-to-rapture/problem
object JackgoestoRapture {

  final case class NodeWeight(node: Int, weight: Int)

  @tailrec
  def dijkstra(
    graph: Map[Int, Set[NodeWeight]],
    curr: Int,
    pending: mutable.PriorityQueue[NodeWeight],
    visited: Set[Int],
    table: Map[Int, Int]): Map[Int, Int] = {
    val currW = table(curr)
    val tb = graph(curr).foldLeft(table) { case (t, edge) =>
      val maxW  = Math.max(currW, edge.weight)
      val exist = table.getOrElse(edge.node, Int.MaxValue)
      if (visited.contains(edge.node)) t
      else {
        pending.enqueue(edge.copy(weight = maxW))
        t + (edge.node -> maxW)
      }
    }
    LazyList
      .unfold(pending.isEmpty)(e => if (e) None else Some((pending.dequeue(), pending.isEmpty)))
      .dropWhile(x => visited.contains(x.node))
      .headOption match {
      case None    => table
      case Some(e) => dijkstra(graph, e.node, pending, visited + e.node + curr, tb)
    }
  }

  def getCost(gNodes: Int, gFromToWeight: Array[Array[Int]]) {
    val graph =
      gFromToWeight.foldLeft(HashMap.empty[Int, Set[NodeWeight]]) { case (m, Array(f, t, w)) =>
        m.updatedWith(f)(_.map(_ + NodeWeight(t, w)).orElse(Some(Set(NodeWeight(t, w)))))
          .updatedWith(t)(_.map(_ + NodeWeight(f, w)).orElse(Some(Set(NodeWeight(f, w)))))
      }

    val res =
      dijkstra(
        graph,
        1,
        mutable.PriorityQueue.empty[NodeWeight](Ordering.by[NodeWeight, Int](_.weight)).reverse,
        Set(1),
        HashMap(1 -> 0)).get(gNodes) match {
        case None    => "NO PATH EXISTS"
        case Some(x) => x.toString
      }
    println(res)
  }

  val start = LocalDateTime.now

  val gFromToWeight = file"/Users/chenh/Downloads/input15.txt".lines.map(_.split(" ").map(_.toInt)).toArray
  val gNodes        = 50000
  getCost(gNodes, gFromToWeight)
  println(Duration.between(LocalDateTime.now, start))

  val arr =
    Array(
      Array(1, 2, 5),
      Array(1, 3, 10),
      Array(2, 3, 15),
      Array(2, 4, 20),
      Array(2, 6, 30),
      Array(4, 6, 10),
      Array(4, 5, 20),
      Array(5, 7, 40),
      Array(6, 7, 50))
  println(getCost(7, arr))

}

object testmain {
  import JackgoestoRapture._
  val gNodes = 0

  val gFromToWeight = for (i <- 0 until gNodes) yield {
    val arr = StdIn.readLine().split(" ").map(_.toInt)
    (arr(0), arr(1), arr(2))
  }

  val graph = gFromToWeight.foldLeft(HashMap.empty[Int, Set[NodeWeight]]) { case (m, (f, t, w)) =>
    m.updatedWith(f)(_.map(_ + NodeWeight(t, w)).orElse(Some(Set(NodeWeight(t, w)))))
      .updatedWith(t)(_.map(_ + NodeWeight(f, w)).orElse(Some(Set(NodeWeight(f, w)))))
  }
}
