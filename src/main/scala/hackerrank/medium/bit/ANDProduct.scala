package hackerrank.medium.bit

// https://www.hackerrank.com/challenges/and-product/problem
object ANDProduct {
  def log2(n: Long): Long = (Math.log10(n) / Math.log10(2)).toLong

  def andProduct(a: Long, b: Long): Long =
    a & ~((1 << log2(a ^ b).toLong) - 1)

}
