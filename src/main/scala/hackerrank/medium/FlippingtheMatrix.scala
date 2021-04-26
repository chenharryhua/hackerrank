package hackerrank.medium

object FlippingtheMatrix {
  // https://www.hackerrank.com/challenges/flipping-the-matrix/problem

  def flippingMatrix(matrix: Array[Array[Int]]): Int = {
    val len = matrix.length
    val res = for {
      i <- (0 until len / 2)
      j <- (0 until len / 2)
    } yield List(matrix(i)(j), matrix(i)(len - j - 1), matrix(len - i - 1)(j), matrix(len - i - 1)(len - j - 1)).max
    res.sum
  }
}
