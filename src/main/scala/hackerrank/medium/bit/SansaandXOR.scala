package hackerrank.medium.bit

// https://www.hackerrank.com/challenges/sansa-and-xor/problem
object SansaandXOR {

  def sansaXor(arr: Array[Int]): Int = {
    val len = arr.length
    if (len % 2 == 0) 0
    else {
      arr.zipWithIndex.filter(_._2 % 2 == 0).foldLeft(0) { case (s, (i, _)) => s ^ i }
    }
  }
}
