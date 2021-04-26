package hackerrank.medium.search

object CuttheTree {

  // https://www.hackerrank.com/challenges/cut-the-tree/problem?h_r=internal-search
  // dfs
  def dfs(tree: Map[Int, Set[Int]], weights: Map[Int, Int], all: Int, curr: Int, prev: Int): (Int, Int) = {
    val next = tree(curr) - prev
    if (next.isEmpty) {
      val w = weights(curr)
      (w, Math.abs(all - 2 * w))
    } else {
      val (s, c) = next.foldLeft((0, Int.MaxValue)) { case ((s, c), i) =>
        val (ss, sc) = dfs(tree, weights, all, i, curr)
        (ss + s, Math.min(sc, c))
      }
      val w = s + weights(curr)
      (w, Math.min(Math.abs(all - 2 * w), c))
    }
  }

  def cutTheTree(data: Array[Int], edges: Array[Array[Int]]): Int = {
    val weights: Map[Int, Int] = data.zipWithIndex.map { case (a, b) => (b + 1, a) }.toMap
    val tree =
      edges.foldLeft(Map.empty[Int, Set[Int]]) { case (m, Array(from, to)) =>
        m.updatedWith(from)(_.map(_ + to).orElse(Some(Set(to))))
          .updatedWith(to)(_.map(_ + from).orElse(Some(Set(from))))
      }

    val all  = data.sum
    val root = tree.keys.min
    dfs(tree, weights, all, root, root)._2
  }

  val edges = Array(
    Array(2, 8),
    Array(10, 5),
    Array(1, 7),
    Array(6, 9),
    Array(4, 3),
    Array(8, 10),
    Array(5, 1),
    Array(7, 6),
    Array(9, 4))
  val data = Array(205, 573, 985, 242, 830, 514, 592, 263, 142, 915)

  println(cutTheTree(data, edges))
}
