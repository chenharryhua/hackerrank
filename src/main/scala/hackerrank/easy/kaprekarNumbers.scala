package hackerrank.easy

object kaprekarNumbers {

  def kaprekarNumbers(p: Int, q: Int): Unit = {
    def isKap(n: Int): Option[Int] = {
      val s      = (n.toLong * n.toLong).toString
      val (h, t) = s.splitAt(s.length / 2)
      val f      = if (h.isEmpty) 0 else h.toInt
      val r      = if (t.isEmpty) 0 else t.toInt
      if ((f + r) == n) Some(n) else None
    }
    val rst = (p to q).flatMap(isKap).toList
    if (rst.isEmpty) println("INVALID RANGE") else println(rst.mkString(" "))
  }

  def beautifulTriplets(d: Int, arr: Array[Int]): Int = {
    val len = arr.length
    val rst = for {
      i <- 0 until len
      j <- i + 1 until len
      k <- j + 1 until len
    } yield if (arr(j) - arr(i) == d && arr(k) - arr(j) == d) 1 else 0
    rst.sum
  }

  def howManyGames(p: Int, d: Int, m: Int, s: Int): Int = {
    val cost = LazyList.from(p, -d).map(x => if (x <= m) m else x)
    cost.scanLeft(s) { case (r, i) => r - i }.takeWhile(_ > 0).length
  }

  def timeInWords(h: Int, m: Int): String = {
    val num = Map(
      1 -> "one",
      2 -> "two",
      3 -> "three",
      4 -> "four",
      5 -> "five",
      6 -> "six",
      7 -> "seven",
      8 -> "eight",
      9 -> "nine",
      10 -> "ten",
      11 -> "eleven",
      12 -> "twelve",
      13 -> "thirteen",
      14 -> "fourteen",
      15 -> "fifteen",
      16 -> "sixteen",
      17 -> "seventeen",
      18 -> "eighteen",
      19 -> "nineteen",
      20 -> "twenty",
      21 -> "twenty one",
      22 -> "twenty two",
      23 -> "twenty three",
      24 -> "twenty four",
      25 -> "twenty five",
      26 -> "twenty six",
      27 -> "twenty seven",
      28 -> "twenty eight",
      29 -> "twenty nine"
    )
    if (m == 0) s"${num(h)} o' clock"
    else if (m == 1) s"one minute past ${num(h)}"
    else if (m == 15) s"quarter past ${num(h)}"
    else if (m == 30) s"half past ${num(h)}"
    else if (m == 45) s"quarter to ${num(h + 1)}"
    else if (m < 30) s"${num(m)} minutes past ${num(h)}"
    else if (m > 30) s"${num(60 - m)} minutes to ${num(h + 1)}"
    else sys.error("oops")
  }

  def chocolateFeast(n: Int, c: Int, m: Int): Int = {
    def chocolate(bar: Int, r: Int, count: Int): Int = {
      val t = bar + r
      if (t >= m) chocolate(t / m, t % m, count + t / m) else count
    }
    chocolate(n / c, 0, 0)
  }

}
