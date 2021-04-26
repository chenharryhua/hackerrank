package hackerrank.hard

// https://www.hackerrank.com/challenges/floyd-city-of-blinding-lights/problem
object CityofBlindingLights {

  def floyd(matrix: Array[Array[Int]]): Array[Array[Int]] = {
    val len = matrix.length
    for {
      k <- 1 until len
      i <- 1 until len
      j <- 1 until len
    } if (matrix(i)(k) != Int.MaxValue &&
      matrix(k)(j) != Int.MaxValue &&
      matrix(i)(k) + matrix(k)(j) < matrix(i)(j)) matrix(i)(j) = matrix(i)(k) + matrix(k)(j)
    matrix
  }

  def mainApp(args: Array[String]) {
    val stdin                       = scala.io.StdIn
    val Array(roadNodes, roadEdges) = stdin.readLine().trim.split(" ").map(_.toInt)
    val matrix                      = Array.fill(roadNodes + 1)(Array.fill[Int](roadNodes + 1)(Int.MaxValue))
    matrix.indices.foreach(i => matrix(i)(i) = 0)
    for (i <- 0 until roadEdges) {
      val roadFromToWeight = stdin.readLine().trim.split(" ").map(_.toInt)
      matrix(roadFromToWeight(0))(roadFromToWeight(1)) = roadFromToWeight(2)
    }

    val q     = stdin.readLine.trim.toInt
    val query = floyd(matrix)

    for (qItr <- 1 to q) {
      val xy  = stdin.readLine.split(" ")
      val x   = xy(0).trim.toInt
      val y   = xy(1).trim.toInt
      val ans = query(x)(y)
      if (ans == Int.MaxValue) println(-1) else println(ans)
    }
  }
}
