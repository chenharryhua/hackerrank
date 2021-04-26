package hackerrank.medium.graph

final class DisjointSet(val nodes: Vector[Option[Int]]) {

  private def root(n: Int): Int = nodes(n) match {
    case None              => n
    case Some(p) if p == n => n
    case Some(p)           => root(p)
  }

  def union(n1: Int, n2: Int): DisjointSet = {
    val root1 = root(n1)
    val root2 = root(n2)
    if (root1 == root2) this
    else {
      val nr = Some(root1)
      new DisjointSet(nodes.updated(root2, nr).updated(n1, nr).updated(n2, nr))
    }
  }
  def clusters = nodes.groupBy(_.map(root))
}

object DisjointSet {

  def apply(size: Int): DisjointSet = {
    val nodes: Vector[Option[Int]] = Vector.fill(size + 1)(None)
    new DisjointSet(nodes)
  }
}
