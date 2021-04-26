package hackerrank.medium.graph

import java.util.UUID

// https://www.hackerrank.com/challenges/kruskalmstrsub/problem
object Kruskal {
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

  def kruskals(gNodes: Int, gFrom: Array[Int], gTo: Array[Int], gWeight: Array[Int]): Int =
    gFrom
      .zip(gTo)
      .zip(gWeight)
      .map { case ((f, t), w) => Node(f, t, w) }
      .sortBy(_.weight)
      .foldLeft(DisjointSet()) { case (s, n) => s.add(n.from, n.to, n.weight) }
      .weight

  val gFrom   = Array(1, 3, 4, 1, 3)
  val gTo     = Array(2, 2, 3, 4, 1)
  val gWeight = Array(1, 150, 99, 100, 200)

  kruskals(0, gFrom, gTo, gWeight)

}
