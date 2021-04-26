package hackerrank.medium.graph

import better.files._

object BreadthFirstSearch {
  // https://www.hackerrank.com/challenges/bfsshortreach/problem

  def height(neighbors: Map[Int, Set[Int]], level: Int, stack: Set[Int], rst: Map[Int, Int]): Map[Int, Int] =
    if (stack.isEmpty) rst
    else {
      val known = stack.filterNot(rst.contains).map((_ -> level)).toMap ++ rst
      val set   = stack.flatMap(neighbors.get).flatten.filterNot(known.contains)
      height(neighbors, level + 1, set, known)
    }

  def bfs(n: Int, m: Int, edges: Array[Array[Int]], s: Int): Array[Int] = {
    val neighbors = edges.foldLeft(Map.empty[Int, Set[Int]]) { case (m, Array(from, to)) =>
      m.updatedWith(from)(_.map(_ + to).orElse(Some(Set(to)))).updatedWith(to)(_.map(_ + from).orElse(Some(Set(from))))
    }
    val map = height(neighbors, 1, neighbors.getOrElse(s, Set()), Map.empty)
    (1 to n).filterNot(_ == s).map(x => map.get(x).map(_ * 6).getOrElse(-1)).toArray
  }

  val data = file"/Users/chenh/Downloads/test.txt".lines.map(_.split(" ").map(_.toInt)).toArray
  println(bfs(70, 0, data, 16).toList)

  // val data = Array(Array(1, 2), Array(1, 3), Array(1, 4), Array(5, 4), Array(3, 4), Array(2, 4), Array(3, 6))
  // println(bfs(7, 0, data, 1).toList)
}
