package hackerrank.medium.search

// https://www.hackerrank.com/challenges/short-palindrome/problem
object ShortPalindrome {
  val M = 1000000007L

  def shortPalindrome(s: String): Long = {
    val l1 = Array.ofDim[Long](26)
    val l2 = Array.ofDim[Long](26, 26)
    val l3 = Array.ofDim[Long](26, 26, 26)
    val l4 = Array.ofDim[Long](26, 26, 26, 26)
    s.toCharArray.foreach { c =>
      val idx = c - 'a'
      (0 until 26).foreach { i =>
        l4(idx)(i)(i)(idx) += l3(idx)(i)(i) % M
        l3(i)(idx)(idx) += l2(i)(idx)       % M
        l2(i)(idx) += l1(i)                 % M
      }
      l1(idx) += 1
    }
    l4.flatMap(_.flatMap(_.flatten)).sum % M
  }

  println(shortPalindrome("kkkkkkz"))
}
