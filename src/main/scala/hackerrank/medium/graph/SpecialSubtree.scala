package hackerrank.medium.graph

import java.util.UUID

// https://www.hackerrank.com/challenges/primsmstsub/problem?h_r=internal-search
object SpecialSubtree {
  final case class Node(from: Int, to: Int, weight: Int)

  final case class DisjointSet(ds: Map[UUID, Set[Int]] = Map.empty, ws: List[Int] = List.empty) {

    def find(v1: Int, v2: Int): Vector[UUID] =
      ds.flatMap { case (k, v) => if (v.contains(v1) || v.contains(v2)) Some(k) else None }.toVector

    def exists(v: Int): Boolean = ds.exists(_._2.contains(v))

    def add(v1: Int, v2: Int, weight: Int): DisjointSet =
      find(v1, v2) match {
        case Vector() => DisjointSet(ds + (UUID.randomUUID() -> Set(v1, v2)), weight :: ws)
        case Vector(a) =>
          val v = ds(a)
          val w = if (v.contains(v1) && v.contains(v2)) ws else weight :: ws
          DisjointSet(ds + (a -> (ds(a).union(Set(v1, v2)))), w)
        case Vector(a, b) =>
          DisjointSet((ds.removed(a).removed(b)) + (UUID.randomUUID() -> (ds(a) ++ ds(b))), weight :: ws)
        case _ => throw new Exception("oops")
      }

    def weight = ws.sum
  }

  def prims(n: Int, edges: Array[Array[Int]], start: Int): Int =
    edges.map { case Array(f, t, w) => Node(f, t, w) }
      .sortBy(_.weight)
      .foldLeft(DisjointSet()) { case (s, n) => s.add(n.from, n.to, n.weight) }
      .weight

}
