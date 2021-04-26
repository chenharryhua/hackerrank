package hackerrank.easy

object gridSearch {

  def fairRations(B: Array[Int]): Int = {
    def odd(n: Int): Boolean = n % 2 == 1
    if (odd(B.sum)) 0
    else {
      B.tail
        .foldLeft((B.head, 0)) { case ((b, s), i) =>
          if (odd(b)) (i + 1, s + 2) else (i, s)
        }
        ._2
    }
  }

  def cavityMap(grid: Array[String]): Array[String] = {
    val ds  = grid.map(_.toCharArray)
    val len = ds(0).length

    for {
      i <- 1 until len - 1
      j <- 1 until len - 1
    } {
      val up    = ds(i)(j) > ds(i)(j - 1)
      val down  = ds(i)(j) > ds(i)(j + 1)
      val right = ds(i)(j) > ds(i + 1)(j)
      val left  = ds(i)(j) > ds(i - 1)(j)
      if (up && down && right && left) ds(i)(j) = 'X'
    }

    ds.map(_.toString)
  }

  val grid = Array("1112", "1912", "1892", "1234")

  def stones(n: Int, a: Int, b: Int): Array[Int] =
    (0 until n).map(x => a * (n - 1 - x) + b * x).sorted.toArray

  def gridSearch(G: Array[String], P: Array[String]): String = {
    val out  = G.map(_.toCharArray)
    val orow = out.length
    val ocol = out(0).length
    val pat  = P.map(_.toCharArray)
    val prow = pat.length
    val pcol = pat(0).length

    def checkSqure(or: Int, oc: Int): Boolean = {
      println(or, oc)
      for {
        i <- (0 until prow)
        j <- (0 until pcol)
      } {
        println(i, j)
        if (pat(i)(j) != out(or + i)(oc + j)) return false
      }
      true
    }

    val rst = for {
      r <- 0 to (orow - prow)
      c <- 0 to (ocol - pcol)
    } yield {
      println(r, c)
      checkSqure(r, c)
    }

    if (rst.exists(identity)) "YES" else "NO"

  }
  val G = Array("999999", "121211")
  val P = Array("99", "11")
  gridSearch(G, P)
}
