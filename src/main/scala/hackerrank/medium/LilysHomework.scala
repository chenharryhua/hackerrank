package hackerrank.medium

import better.files._

// https://www.hackerrank.com/challenges/lilys-homework/problem
object LilysHomework {

  def solution(arr: Array[Int]): Int = {
    val sorted = arr.sorted
    var m      = arr.zipWithIndex.toMap

    sorted.zipWithIndex.map { case (d, idx) =>
      if (d == arr(idx)) 0
      else {
        val tmp = m(d)
        m ++= Map(arr(idx) -> tmp, d -> idx)
        arr(tmp) = arr(idx)
        arr(idx) = d
        1
      }
    }.sum
  }

  def lilysHomework(arr: Array[Int]): Int = {
    val clone = arr.clone()
    val k     = Math.min(solution(arr), solution(clone.reverse))
    (arr.toList.foreach(println))
    k
  }

  println(lilysHomework(file"/Users/chenh/Downloads/test.txt".contentAsString.split(" ").map(_.toInt)))

}
