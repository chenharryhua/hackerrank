package hackerrank.medium.graph

object RoadsandLibraries {
  //https://www.hackerrank.com/challenges/torque-and-development/problem

  def roadsAndLibraries(n: Int, c_lib: Int, c_road: Int, cities: Array[Array[Int]]): Long =
    if (c_lib < c_road) n * c_lib.toLong
    else {
      cities.foldLeft(DisjointSet(n)) { case (ds, Array(f, t)) => ds.union(f, t) }.clusters.foldLeft(0L) {
        case (s, (i, v)) =>
          (i match {
            case None    => (v.size - 1) * c_lib.toLong
            case Some(_) => (v.size - 1) * c_road.toLong + c_lib
          }) + s
      }
    }

  val cities = Array(Array(1, 2), Array(3, 1), Array(2, 3))
  val kkk    = roadsAndLibraries(3, 2, 1, cities)
  val ggg    = cities.foldLeft(DisjointSet(3)) { case (ds, Array(f, t)) => ds.union(f, t) }.nodes

  println(kkk)
}
