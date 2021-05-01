package hackerrank.medium.dp

// https://www.hackerrank.com/challenges/xor-and-sum/problem
object XorandSum extends App {

  def xorAndSum(a: String, b: String): Long = {
    val aa = BigInt(Integer.parseInt(a, 2))
    val bb = BigInt(Integer.parseInt(b, 2))

    (0 to 314159).foldLeft(0L) { case (s, i) =>
      ((s + (aa ^ (bb << i))) % 1000000007L).toLong
    }
  }

  println(xorAndSum("10", "1010"))
}
