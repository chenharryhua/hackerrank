package hackerrank.medium.graph

import java.io.PrintWriter
import scala.collection.immutable.TreeMap
import scala.collection.mutable
import better.files._

// https://www.hackerrank.com/challenges/johnland/problem
object RoadsinHackerLand {

  def prim(
    graph: Map[Int, Map[Int, Long]], // undirect graph
    visited: Set[Int],
    queue: mutable.PriorityQueue[(Int, Int, Long)],
    tree: Map[Int, Map[Int, Long]]): Map[Int, Map[Int, Long]] =
    LazyList
      .unfold(queue.nonEmpty)(s => if (s) Some(queue.dequeue(), queue.nonEmpty) else None)
      .dropWhile { case (_, t, _) => visited(t) }
      .headOption match {
      case None => tree
      case Some((of, ot, ow)) =>
        graph(ot).removed(of).foreach { case (t, w) => queue.enqueue((ot, t, w)) }
        val update = tree.updatedWith(of)(_.map(_.updatedWith(ot)(_ => Some(ow))).orElse(Some(Map(ot -> ow))))
        prim(graph, visited + ot, queue, update)
    }

  def dfs(tree: Map[Int, Map[Int, Long]], nodes: Int, curr: Int, weight: Long): (Long, TreeMap[Long, Long]) =
    tree.get(curr) match {
      case None => (1, TreeMap(weight -> (nodes - 1)))
      case Some(m) =>
        val (cc, cw) = m.foldLeft((0L, TreeMap.empty[Long, Long])) { case ((c, s), (n, w)) =>
          val (nc, ns) = dfs(tree, nodes, n, w)
          (c + nc, s ++ ns)
        }
        val nc = cc + 1
        (nc, cw + (weight -> (nodes - nc) * nc))
    }

  def roadsInHackerland(n: Int, roads: Map[Int, Map[Int, Long]]): String = {
    val first = roads(1).map(x => (1, x._1, x._2))
    val mst =
      prim(
        roads,
        Set(1),
        mutable.PriorityQueue.from(first)(Ordering.by[(Int, Int, Long), Long](_._3).reverse),
        Map.empty)
    val weights = dfs(mst, n, 1, -1)._2.removed(-1)
    weights.foldLeft(BigInt(0)) { case (s, (i, n)) => s + (BigInt(1) << i.toInt) * n }.toString(2)
  }

  val m =
    Map(
      1 -> Map(3 -> 5, 2 -> 3),
      2 -> Map(1 -> 3, 3 -> 1, 4 -> 2),
      3 -> Map(1 -> 5, 2 -> 1, 4 -> 4),
      4 -> Map(3 -> 4, 2 -> 2, 5 -> 0),
      5 -> Map(4 -> 0))

  val rr = file"/Users/chenh/Downloads/input15.txt".lines
    .map(_.split(" ").map(_.toInt))
    .flatMap { r =>
      Array((r(0), r(1), r(2).toLong), (r(1), r(0), r(2).toLong))
    }
    .groupBy(_._1)
    .map { case (f, es) => (f -> es.groupBy(_._2).map { case (t, w) => (t -> w.minBy(_._3)._3) }) }

  val ans = roadsInHackerland(100000, rr)

  val out = file"/Users/chenh/Downloads/output15.txt".lines.head

  println(ans.length, out.length)
  val diff = ans.zip(out).zipWithIndex.flatMap { case ((a, b), c) => if (a == b) None else Some(c) }
  println(diff)

  def mainApp(args: Array[String]) {
    val stdin = scala.io.StdIn

    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val nm = stdin.readLine.split(" ")

    val n = nm(0).trim.toInt

    val m = nm(1).trim.toInt

    val rd = (0 until m).flatMap { _ =>
      val r = stdin.readLine().trim.split(" ").map(_.toInt)
      Array((r(0), r(1), r(2).toLong), ((r(1), r(0), r(2).toLong)))
    }

    val roads =
      rd.groupBy(_._1).map { case (f, es) => (f -> es.groupBy(_._2).map { case (t, w) => (t -> w.minBy(_._3)._3) }) }

    val result = roadsInHackerland(n, roads)

    printWriter.println(result)

    printWriter.close()
  }
}
