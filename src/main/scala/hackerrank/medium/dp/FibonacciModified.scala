package hackerrank.medium.dp

object FibonacciModified {

  // https://www.hackerrank.com/challenges/fibonacci-modified/problem
  def fibonacciModified(t1: Int, t2: Int, n: Int): BigInt =
    LazyList
      .iterate((BigInt(t1), BigInt(t2))) { case (f, s) =>
        val s1 = f + s * s
        val s2 = s + s1 * s1
        (s1, s2)
      }
      .flatMap(x => List(x._1, x._2))
      .take(n)
      .last

}
