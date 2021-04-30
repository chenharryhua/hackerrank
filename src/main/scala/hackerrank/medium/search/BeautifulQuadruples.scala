package hackerrank.medium.search

// https://www.hackerrank.com/challenges/xor-quadruples/problem
object BeautifulQuadruples {
  val MaxV = 5000
  val MaxN = 3001

  def beautifulQuadruples(a: Int, b: Int, c: Int, d: Int): Long = {
    val c1 = Array.fill(MaxV)(Array.fill(MaxN)(0))
    val c2 = Array.fill(MaxN)(0)

    val List(w, x, y, z) = List(a, b, c, d).sorted

    for {
      i <- 1 to w
      j <- i to x
    } {
      c1(i ^ j)(j) += 1
      c2(j) += 1
    }
    for {
      i <- 0 until MaxV
      j <- 1 until MaxN
    } yield c1(i)(j) = c1(i)(j) + c1(i)(j - 1)
    for (i <- 1 until MaxN) c2(i) = c2(i) + c2(i - 1)

    var tc    = 0L
    var count = 0L
    for {
      i <- 1 to y
      j <- i to z
    } {
      tc += c2(i)
      count += c1(i ^ j)(i)
    }
    tc - count
  }
}
