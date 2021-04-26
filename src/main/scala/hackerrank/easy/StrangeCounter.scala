package hackerrank.easy

//https://www.hackerrank.com/challenges/strange-code/problem
object StrangeCounter {

  def strangeCounter(t: Long): Long = {
    val (sv, st, et) = LazyList
      .from(0)
      .map(x => 3 * Math.pow(2, x).toLong)
      .zipWithIndex
      .scanLeft((0L, 0L, 0L)) { case ((a, b, c), (d, i)) =>
        (d, c + 1, c + d)
      }
      .find { case (a, b, c) => t >= b && t <= c }
      .get
    sv - (t - st)
  }

}
