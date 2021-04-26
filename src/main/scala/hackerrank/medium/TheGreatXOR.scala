package hackerrank.medium

// https://www.hackerrank.com/challenges/the-great-xor/problem
object TheGreatXOR {

  def theGreatXor(x: Long): Long = {
    val ans = Math.pow(2, (x.toBinaryString.length)) - x - 1
    ans.toLong
  }
}
