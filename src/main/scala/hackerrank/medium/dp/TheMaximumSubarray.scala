package hackerrank.medium.dp

object TheMaximumSubarray {

  //https://www.hackerrank.com/challenges/maxsubarray/problem
  def maxSubarray(arr: Array[Int]): Array[Int] = {
    val ss = if (arr.forall(_ < 0)) arr.max else arr.filter(_ > 0).sum

    val sa = {
      if (arr.forall(_ < 0)) arr.max
      else {
        arr
          .foldLeft((0, 0)) { case ((top, v), i) =>
            val sum = v + i
            if (sum > 0) {
              if (sum > top) (sum, sum) else (top, sum)
            } else (top, 0)
          }
          ._1
      }
    }

    Array(sa, ss)
  }
}
