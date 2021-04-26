package hackerrank.easy

import better.files._

object almostSorted {

  def turnPoints(arr: Array[Int]): List[Int] = arr.zipWithIndex
    .foldLeft((Int.MinValue, true, List.empty[Int])) { case ((last, direction, s), (d, idx)) =>
      if (d > last && direction) (d, direction, s)
      else if (d < last && !direction) (d, direction, s)
      else (d, !direction, idx :: s)
    }
    ._3
    .reverse

  def swap(x: Int, y: Int, arr: Array[Int]): Boolean = {
    val tmp = arr(x)
    arr(x) = arr(y)
    arr(y) = tmp
    turnPoints(arr).isEmpty
  }

  def reverse(x: Int, y: Int, arr: Array[Int]): Boolean = {
    val h = arr.take(x)
    val t = arr.takeRight(arr.length - y - 1)
    val m = arr.drop(x).dropRight(arr.length - y - 1)
    turnPoints(h ++ m.reverse ++ t).isEmpty
  }

  def almostSorted(arr: Array[Int]) {
    println(turnPoints(arr))
    turnPoints(arr) match {
      case Nil => println("yes")
      case a :: Nil =>
        if (swap(a - 1, a, arr)) {
          println("yes")
          println(s"swap ${a} ${a + 1}")
        } else println("no")

      case a :: b :: Nil =>
        if (a + 1 == b && swap(a - 1, b - 1, arr)) {
          println("yes")
          println(s"swap $a $b")
        } else if (reverse(a - 1, b - 1, arr)) {
          println("yes")
          println(s"reverse $a $b")
        } else println("no")

      case a :: b :: c :: d :: Nil =>
        if (a + 1 == b && c + 1 == d && swap(a - 1, d - 1, arr)) {
          println("yes")
          println(s"swap ${a} ${d}")
        } else println("no")
      case _ => println("no")
    }
  }

  almostSorted(file"/Users/chenh/Downloads/input07.txt".contentAsString.split(" ").map(_.toInt))

  val g1 = Array(1, 5, 4, 3, 2, 6)
}
