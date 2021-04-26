package hackerrank.medium.graph

import java.util.UUID

// https://www.hackerrank.com/challenges/journey-to-the-moon/problem
object JourneyToMoon {

  final case class DisjointSet(ds: Map[UUID, Set[Int]] = Map.empty) {

    def find(v1: Int, v2: Int): Vector[UUID] =
      ds.flatMap { case (k, v) => if (v.contains(v1) || v.contains(v2)) Some(k) else None }.toVector

    def exists(v: Int): Boolean = ds.exists(_._2.contains(v))

    def add(v1: Int, v2: Int): DisjointSet =
      find(v1, v2) match {
        case Vector()     => DisjointSet(ds + (UUID.randomUUID() -> Set(v1, v2)))
        case Vector(a)    => DisjointSet(ds + (a -> (ds(a).union(Set(v1, v2)))))
        case Vector(a, b) => DisjointSet((ds.removed(a).removed(b)) + (UUID.randomUUID() -> (ds(a) ++ ds(b))))
        case _            => throw new Exception("oops")
      }
  }

  def journeyToMoon(n: Int, astronaut: Array[Array[Int]]): Int = {
    val known = astronaut.foldLeft(DisjointSet()) { case (m, Array(f, s)) => m.add(f, s) }

    val ds = Vector.fill(n - known.ds.map(_._2.size).sum)(1) ++ known.ds.map(_._2.size)

    ds.foldLeft((0, 0)) { case ((s, r), i) => (s + i, r + s * i) }._2

  }

  val astronaut = Array(Array(0, 2), Array(1, 8), Array(1, 4), Array(2, 8), Array(2, 6), Array(3, 5), Array(6, 9))
}
