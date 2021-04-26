package hackerrank.medium.graph

import scala.collection.mutable

//https://www.hackerrank.com/challenges/crab-graphs/problem
object CrabGraphs {

  final case class EdgeID(from: Int, to: Int) {
    lazy val swap: EdgeID = EdgeID(to, from)
  }
  final case class Edge(id: EdgeID, capacity: Int)

  def maxFlow(graph: Map[Int, Set[Int]], start: Int, end: Int, residual: Map[EdgeID, Int]): Map[EdgeID, Int] = {
    def augmentPath(
      residual: Map[EdgeID, Int],
      visited: Set[Int],
      queue: mutable.Queue[(Int, List[EdgeID])]
    ): List[EdgeID] =
      LazyList
        .unfold(queue.nonEmpty)(s => if (s) Some(queue.dequeue(), queue.nonEmpty) else None)
        .dropWhile(x => visited(x._1))
        .headOption match {
        case None => List()
        case Some((n, p)) =>
          if (n == end) p
          else {
            val q = graph(n).filter(x => residual(EdgeID(n, x)) > 0).map(x => (x, EdgeID(n, x) :: p))
            queue.enqueueAll(q)
            augmentPath(residual, visited + n, queue)
          }
      }
    val ap = augmentPath(residual, Set.empty, mutable.Queue((start, List.empty)))
    ap.map(residual).minOption match {
      case None => residual
      case Some(x) =>
        val update = ap.foldLeft(residual) { case (r, id) =>
          r.updatedWith(id)(_.map(_ - x)).updatedWith(id.swap)(_.map(_ + x))
        }
        maxFlow(graph, start, end, update)
    }
  }

  def crabGraphs(n: Int, t: Int, raw: Array[Edge]): Int = {

    val sourceEdge  = raw.groupBy(_.id.from).map { case (f, e) => Edge(EdgeID(0, f), Math.min(t, e.length)) }
    val sinkEdge    = raw.groupBy(_.id.to).map { case (t, _) => Edge(EdgeID(t, Int.MaxValue), 1) }
    val reverseEdge = raw.map(x => Edge(x.id.swap, 0))
    val allEdges    = raw ++ reverseEdge ++ sourceEdge ++ sinkEdge
    val residual    = allEdges.groupBy(_.id).map { case (id, e) => assert(e.length == 1); (id, e.head.capacity) }

    val nodes = (allEdges.map(_.id)).groupBy(_.from).map { case (f, e) => (f -> e.map(_.to).toSet) }
    if (nodes.isEmpty) 0
    else {
      val max = maxFlow(nodes, 0, Int.MaxValue, residual)
      max.count(x => x._1.to == Int.MaxValue && x._2 == 0)
    }
  }

  val arr = Array(Array[Int]())

  val raw: Array[Edge] = arr.flatMap { r =>
    val e1 = EdgeID(r(0) * 2, r(1) * 2 - 1)
    val e2 = EdgeID(r(1) * 2, r(0) * 2 - 1)
    Array(Edge(e1, 1), Edge(e2, 1))
  }

  println(crabGraphs(50, 3, raw))
}
