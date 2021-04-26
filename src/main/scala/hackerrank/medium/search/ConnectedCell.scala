package hackerrank.medium.search

object ConnectedCell {

  // https://www.hackerrank.com/challenges/connected-cell-in-a-grid/problem
  def connectedCell(matrix: Array[Array[Int]]): Int = {
    val rows  = matrix.length
    val cols  = matrix(0).length
    val arr   = matrix.map(x => 0 :: x.toList ::: List(0)).toList
    val hf    = List.fill(cols + 2)(0)
    val board = hf :: arr ::: List(hf)
    val neighbors: IndexedSeq[Set[(Int, Int)]] = for {
      r <- 1 to rows
      c <- 1 to cols if board(r)(c) == 1
    } yield Set(
      (r, c),
      (r, c - 1),
      (r, c + 1),
      (r - 1, c),
      (r - 1, c - 1),
      (r - 1, c + 1),
      (r + 1, c),
      (r + 1, c - 1),
      (r + 1, c - 1)).filterNot { case (r, c) => board(r)(c) == 0 }

    val len = neighbors.length

    (0 until len).map { x =>
      neighbors
        .drop(x)
        .reduceLeft((a, b) =>
          if ((a.intersect(b).nonEmpty)) {
            a.union(b)
          } else if (a.size >= b.size) a
          else b)
        .size
    }.max

  }

  def isValid(s: String): String = {
    val rst = s.groupBy(identity).map { case (a, b) => (a, b.length) }
    val ks  = rst.map(_.swap).keys.toList.sorted.reverse

    "YES"
  }

  val matrix = Array(
    Array(0, 1, 0, 0, 0, 0, 1, 1, 0),
    Array(1, 1, 0, 0, 1, 0, 0, 0, 1),
    Array(0, 0, 0, 0, 1, 0, 1, 0, 0),
    Array(0, 1, 1, 1, 0, 1, 0, 1, 1),
    Array(0, 1, 1, 1, 0, 0, 1, 1, 0),
    Array(0, 1, 0, 1, 1, 0, 1, 1, 0),
    Array(0, 1, 0, 0, 1, 1, 0, 1, 1),
    Array(1, 0, 1, 1, 1, 1, 0, 0, 0)
  )
}
