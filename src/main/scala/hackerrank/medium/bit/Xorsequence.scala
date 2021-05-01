package hackerrank.medium.bit

// https://www.hackerrank.com/challenges/xor-se/problem
object Xorsequence {

  def go(x: BigInt): BigInt = {
    val a = x % 8
    if (a == 0 || a == 1) x
    else if (a == 2 || a == 3) 2
    else if (a == 4 || a == 5) x + 2
    else if (a == 6 || a == 7) 0
    else 0
  }

  def xorSequence(l: Long, r: Long): Long =
    (go(r) ^ go(l - 1)).toLong

}
