package hackerrank.medium.graph

object EvenTree {

  // https://www.hackerrank.com/challenges/even-tree/problem
  def nodes(tree: Map[Int, Set[Int]], root: Int): Int =
    tree.get(root) match {
      case None    => 1
      case Some(s) => s.foldLeft(1) { case (m, i) => m + nodes(tree, i) }
    }

  def evenForest(t_nodes: Int, t_edges: Int, t_from: Array[Int], t_to: Array[Int]): Int = {

    val tree: Map[Int, Set[Int]] = t_to.zip(t_from).foldLeft(Map.empty[Int, Set[Int]]) { case (m, (f, t)) =>
      m.updatedWith(f)(_.map(_ + t).orElse(Some(Set(t))))
    }

    t_to.groupBy(identity).map { case (k, _) => nodes(tree, k) }.count(_ % 2 == 0) - 1

  }

  val t_from = Array(2, 3, 4, 5, 6, 7, 8, 9, 10)
  val t_to   = Array(1, 1, 3, 2, 1, 2, 6, 8, 8)
}
