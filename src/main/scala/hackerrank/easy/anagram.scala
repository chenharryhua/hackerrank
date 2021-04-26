package hackerrank.easy

object anagram {

  def anagram(s: String): Int =
    if (s.length % 2 == 1) -1
    else {
      val (h, t) = s.splitAt(s.length / 2)
      val hc     = h.groupBy(identity).map { case (a, b) => (a, b.length) }
      val tc     = t.groupBy(identity).map { case (a, b) => (a, b.length) }
      tc.toList.map { case (c, count) => hc.get(c).map(x => Math.max(0, count - x)).getOrElse(count) }.sum
    }

  def makingAnagrams(s1: String, s2: String): Int = {
    val hc = s1.groupBy(identity).map { case (a, b) => (a, b.length) }
    val tc = s2.groupBy(identity).map { case (a, b) => (a, b.length) }
    tc.toList.map { case (c, count) => hc.get(c).map(x => Math.max(0, count - x)).getOrElse(count) }.sum +
      hc.toList.map { case (c, count) => tc.get(c).map(x => Math.max(0, count - x)).getOrElse(count) }.sum

  }

  def beautifulBinaryString(b: String): Int =
    (b.length() - b.replaceAll("010", "").length) / 3

  def theLoveLetterMystery(s: String): Int = {
    val arr = s.toCharArray
    val len = arr.length
    (0 until len / 2).map(x => Math.abs(arr(x) - arr(len - x))).sum
  }

  def gameOfThrones(s: String): String = {

    val t = s.groupBy(identity).map { case (a, b) => b.length % 2 == 1 }.count(identity)

    if (t > 1) "NO" else "YES"
  }

  def marcsCakewalk(calorie: Array[Int]): Long =
    calorie.sorted.foldRight((0, 0L)) { case (i, (p, s)) => (p + 1, s + i * Math.pow(2, p).toLong) }._2

  def lonelyinteger(a: Array[Int]): Int =
    a.groupBy(identity).map { case (a, b) => (a, b.length) }.filterNot(_._2 == 2).head._1

  def maximumToys(prices: Array[Int], k: Int): Int =
    prices.sorted
      .foldLeft(List.empty[Int]) { case (s, i) =>
        if (s.sum >= k) s else i :: s
      }
      .length

  def maximizingXor(l: Int, r: Int): Int = {
    val rst = for {
      a <- l to r
      b <- l to r
    } yield a ^ b

    rst.max

  }

  def twoArrays(k: Int, A: Array[Int], B: Array[Int]): String = {
    val c = A.sorted.zip(B.sorted).map { case (a, b) => (a + b) >= k }.forall(identity)
    if (c) "YES" else "NO"
  }

}
