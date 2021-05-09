package hackerrank.medium.dp

// https://www.hackerrank.com/challenges/equal/problem
object Equal {

  def equal(arr: Array[Int]): Int = {
    val sorted = arr.sorted
    (0 until 3).map { base =>
      sorted.foldLeft(0) { case (s1, c) =>
        val delta = c - sorted(0) + base
        s1 + delta / 5 + (delta % 5) / 2 + (delta % 5) % 2
      }
    }.min
  }
}
