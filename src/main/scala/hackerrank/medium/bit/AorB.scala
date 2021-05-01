package hackerrank.medium.bit

// https://www.hackerrank.com/challenges/aorb/problem
object AorB extends App {

  def aOrB(k: Int, a: String, b: String, c: String) {
    val asr = BigInt(a, 16).toString(2).toCharArray.map(_ - 48)
    val bsr = BigInt(b, 16).toString(2).toCharArray.map(_ - 48)
    val csr = BigInt(c, 16).toString(2).toCharArray.map(_ - 48)

    val maxLen = Math.max(asr.length, Math.max(bsr.length, csr.length))

    val as = Array.fill(maxLen - asr.length)(0) ++ asr
    val bs = Array.fill(maxLen - bsr.length)(0) ++ bsr
    val cs = Array.fill(maxLen - csr.length)(0) ++ csr

    val (sum, la, lb) =
      as.zip(bs).zip(cs).foldRight((0, List.empty[Int], List.empty[Int])) { case (((a, b), c), (s, la, lb)) =>
        if ((a | b) != c) {
          (a, b, c) match {
            case (1, 1, 0) => (s + 2, 0 :: la, 0 :: lb)
            case (_, _, 0) => (s + 1, 0 :: la, 0 :: lb)
            case (_, _, 1) => (s + 1, 0 :: la, 1 :: lb)
          }
        } else
          (s, a :: la, b :: lb)
      }

    if (sum > k) println("-1")
    else {
      val (sss, ra, rb) =
        la.zip(lb).foldLeft((k - sum, List.empty[Int], List.empty[Int])) { case ((s, la, lb), (a, b)) =>
          if (s == 0) (s, a :: la, b :: lb)
          else if (s == 1) {
            if (a == 1 && b == 1) (0, 0 :: la, b :: lb) else (s, a :: la, b :: lb)
          } else {
            if (a == 1 && b == 1) (s - 1, 0 :: la, b :: lb)
            else if (a == 1 && b == 0) (s - 2, 0 :: la, 1 :: lb)
            else (s, a :: la, b :: lb)
          }
        }
      println(BigInt(ra.reverse.mkString, 2).toString(16).toUpperCase())
      println(BigInt(rb.reverse.mkString, 2).toString(16).toUpperCase())
    }
  }

  println(aOrB(25, "B631EB5AE", "601C227E1", "707AC8792"))
}
