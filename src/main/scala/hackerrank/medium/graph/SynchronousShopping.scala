package hackerrank.medium.graph

import java.io._
import java.time.LocalDateTime
import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.io._

//https://www.hackerrank.com/challenges/synchronous-shopping/problem
object SynchronousShopping {
  val start = LocalDateTime.now
  final case class NodeFish(node: Int, fish: Int)
  final case class NodeFishWeight(nf: NodeFish, weight: Int)

  @tailrec
  def dijkstra(
    graph: Array[Set[(Int, Int)]],
    nodeFish: HashMap[Int, Int],
    pending: mutable.PriorityQueue[NodeFishWeight],
    visited: Set[NodeFish],
    table: Array[Array[Int]]): Array[Array[Int]] =
    LazyList
      .unfold(pending.isEmpty)(e => if (e) None else Some((pending.dequeue(), pending.isEmpty)))
      .dropWhile(x => visited.contains(x.nf))
      .headOption match {
      case None => table
      case Some(curr) =>
        graph(curr.nf.node).foreach { case (n, w) =>
          val fish = nodeFish(n) | curr.nf.fish
          val tW   = table(n)(fish)
          val iW   = curr.weight + w
          if (iW < tW && !visited.contains(NodeFish(n, iW))) {
            pending.enqueue(NodeFishWeight(NodeFish(n, fish), iW))
            table(n)(fish) = iW
          }
        }
        dijkstra(graph, nodeFish, pending, visited + curr.nf, table)
    }

  def shop(n: Int, k: Int, centers: HashMap[Int, Int], roads: Array[Set[(Int, Int)]]) = {
    val head = NodeFish(1, centers(1))
    val tb   = Array.fill(n + 1)(Array.fill(Math.pow(2, k).toInt)(Int.MaxValue))
    val paths = dijkstra(
      roads,
      centers,
      mutable.PriorityQueue(NodeFishWeight(head, 0))(Ordering.by[NodeFishWeight, Int](_.weight)).reverse,
      Set.empty,
      tb
    )(n).zipWithIndex.map { case (w, i) => ((i | centers(n)) -> w) }
    val ans = for {
      p1 <- paths
      p2 <- paths if (p1._1 | p2._1) == Math.pow(2, k).toInt - 1
    } yield Math.max(p1._2, p2._2)
    ans.minOption match {
      case None    => -1
      case Some(x) => x
    }
  }

  import better.files._

  val rr = file"/Users/chenh/Downloads/roads.txt".lines.map(_.split(" ").map(_.toInt)).toArray

  val roads = rr.foldLeft(HashMap.empty[Int, HashMap[Int, Int]]) { case (m, Array(f, t, w)) =>
    m.updatedWith(f)(_.map(_ + (t -> w)).orElse(Some(HashMap(t -> w))))
      .updatedWith(t)(_.map(_ + (f -> w)).orElse(Some(HashMap(f -> w))))
  }

  val centers =
    file"/Users/chenh/Downloads/centers.txt".lines.map(_.split(" ")).zipWithIndex.foldLeft(HashMap.empty[Int, Int]) {
      case (m, (s, idx)) =>
        val mask = s.tail.foldLeft(0) { case (s, i) =>
          s | 1 << (i.toInt - 1)
        }
        m + (idx + 1 -> mask)
    }

}

object Result {
  final case class NodeFish(node: Int, fish: Int)
  final case class NodeFishWeight(nf: NodeFish, weight: Int)

  @tailrec
  def dijkstra(
    graph: Array[Set[(Int, Int)]],
    nodeFish: Array[Int],
    pending: mutable.PriorityQueue[NodeFishWeight],
    visited: Set[NodeFish],
    table: Array[Array[Int]]): Array[Array[Int]] =
    LazyList
      .unfold(pending.isEmpty)(e => if (e) None else Some((pending.dequeue(), pending.isEmpty)))
      .dropWhile(x => visited.contains(x.nf))
      .headOption match {
      case None => table
      case Some(curr) =>
        graph(curr.nf.node).foreach { case (n, w) =>
          val fish = nodeFish(n) | curr.nf.fish
          val tW   = table(n)(fish)
          val iW   = curr.weight + w
          if (iW < tW) {
            pending.enqueue(NodeFishWeight(NodeFish(n, fish), iW))
            table(n)(fish) = iW
          }
        }
        dijkstra(graph, nodeFish, pending, visited + curr.nf, table)
    }

  def shop(n: Int, k: Int, centers: Array[Int], roads: Array[Set[(Int, Int)]]) = {
    val head = NodeFish(1, centers(1))
    val m    = Math.pow(2, k).toInt
    val tb   = Array.fill(n + 1)(Array.fill(m)(Int.MaxValue))
    val mask = Math.pow(2, k).toInt - 1
    val paths = dijkstra(
      roads,
      centers,
      mutable.PriorityQueue(NodeFishWeight(head, 0))(Ordering.by[NodeFishWeight, Int](_.weight)).reverse,
      Set.empty,
      tb
    )(n).zipWithIndex.map { case (w, i) => ((i | centers(n)) -> w) }
    val len = paths.length
    val ans = for {
      i <- 0 until len
      j <- i + 1 until len if (paths(i)._1 | paths(j)._1) == mask
    } yield Math.max(paths(i)._2, paths(j)._2)
    ans.minOption.getOrElse(-1)
  }
}

object Solution {

  def fmain(args: Array[String]) {

    val printWriter        = new PrintWriter(sys.env("OUTPUT_PATH"))
    val firstMultipleInput = StdIn.readLine.split(" ")
    val n                  = firstMultipleInput(0).toInt
    val m                  = firstMultipleInput(1).toInt
    val k                  = firstMultipleInput(2).toInt

    val centers = Array.fill(n + 1)(0)
    val cs = for (i <- 0 until n) yield {
      val mask = StdIn.readLine.split(" ").map(_.toInt - 1).tail.foldLeft(0) { case (s, i) => s | 1 << i }
      centers(i + 1) = mask
    }
    val roads = Array.fill(m + 1)(Set.empty[(Int, Int)])

    for (i <- 0 until m) {
      val r = StdIn.readLine.split(" ").map(_.toInt)
      roads(r(0)) += (r(1) -> r(2))
      roads(r(1)) += (r(0) -> r(2))
    }

    val res = Result.shop(n, k, centers, roads)

    printWriter.println(res)

    printWriter.close()

  }
}
