package hackerrank.easy

object surfaceArea {

  def happyLadybugs(b: String): String = {
    val cs = b.toCharArray
    val m  = cs.groupBy(identity).map { case (a, b) => (a, b.length) }
    if (m.filterNot(_._1 == '_').values.toList.contains(1)) "NO"
    else if (!m.keys.toList.contains('_')) {
      val r = (1 until cs.length - 1).map(x => cs(x) == cs(x - 1) || cs(x) == cs(x + 1)).forall(identity)
      if (r) "YES" else "NO"
    } else {
      "YES"
    }
  }

  def surfaceArea(A: Array[Array[Int]]): Int = {
    val rows  = A.length
    val cols  = A(0).length
    val arr   = A.map(x => 0 :: x.toList ::: List(0)).toList
    val hf    = List.fill(cols + 2)(0)
    val board = hf :: arr ::: List(hf)

    val rst = for {
      r <- 1 to rows
      c <- 1 to cols
    } yield Math.max(0, board(r)(c) - board(r - 1)(c)) +
      Math.max(0, board(r)(c) - board(r + 1)(c)) +
      Math.max(0, board(r)(c) - board(r)(c - 1)) +
      Math.max(0, board(r)(c) - board(r)(c + 1))
    rst.sum + 2 * rows * cols
  }

  def absolutePermutation(n: Int, k: Int): Array[Int] = {
    val arr = (1 to n).toArray
    if (k == 0) arr
    else if (k * (n / k) != n) Array(-1)
    else if ((n / k) % 2 == 1) Array(-1)
    else {
      arr.grouped(k).toList.grouped(2).foldLeft(Array.empty[Int]) { case (s, i) => s ++ i(1) ++ i.head }
    }
  }

}
