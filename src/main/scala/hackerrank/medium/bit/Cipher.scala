package hackerrank.medium.bit

// https://www.hackerrank.com/challenges/cipher/problem
object Cipher {

  def cipher(k: Int, s: String): String = {
    val arr = s.toCharArray.map(_ - 48)
    val ans = Array.ofDim[Int](s.length)
    arr.indices.foreach { i =>
      if (i == 0) ans(i) = arr(i)
      else if (i < k) ans(i) = arr(i) ^ arr(i - 1)
      else ans(i) = arr(i) ^ arr(i - 1) ^ ans(i - k)
    }
    ans.take(s.length - k + 1).mkString
  }
}
