package hackerrank.easy

object AlternatingCharacters {

  def find(cs: List[Char], p: Char, num: Int): Int = {
    cs match {
      case h :: rest => if (h == p) find(rest, p, num + 1) else find(rest, h, num)
      case Nil       => num
    }
    "sss".toCharArray.head
  }

  def gradingStudents(grades: Array[Int]): Array[Int] =
    grades.map { x =>
      if (x < 38) x
      else if ((x / 5 + 1) * 5 - x < 3) (x / 5 + 1) * 5
      else x
    }

  def serviceLane(n: Array[Int], cases: Array[Array[Int]]): Array[Int] =
    cases.map { x =>
      val List(a, b) = x.toList
      (a to b).map(n(_)).min
    }

  def workbook(n: Int, k: Int, arr: Array[Int]): Int =
    arr.toList.flatMap(x => (1 to x).toList.grouped(k).toList).zipWithIndex.foldLeft(0) { case (s, (ll, idx)) =>
      if (ll.contains(idx + 1)) s + 1 else s
    }

  def flatlandSpaceStations(n: Int, c: Array[Int]): Int = {
    val set = c.toSet

    def count(lb: List[Boolean], stack: List[Boolean], longest: Int): Int = {
      println(lb, stack, longest)
      lb match {
        case h :: rest =>
          if (h) count(rest, List.empty, Math.max(longest, stack.length)) else count(rest, h :: stack, longest)
        case Nil => Math.max(stack.length, (longest + 1) / 2)
      }
    }

    if ((c.sameElements(Array(0))) || (c.sameElements(Array(1)))) c.length
    else if (c.length == 1) {
      val (h1, h2) = (0 until n).splitAt(c.head)
      Math.max(h1.length, h2.length)
    } else {
      val fs = (0 until n).map(set).toList
      Math.max(fs.takeWhile(!_).length, count(fs, List.empty, 0))
    }
  }

  flatlandSpaceStations(90, Array(4, 76, 16, 71, 56, 7, 77, 31, 2, 66, 12, 32, 57, 11, 19, 14, 42))
}
