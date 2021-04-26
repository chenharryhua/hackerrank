package hackerrank.easy

import scala.annotation.tailrec

object WeightedUniformStrings {
  val weight = "abcdefghijklmnopqrstuvwxyz".toCharArray.zipWithIndex.map { case (c, i) => (c, i + 1) }.toMap

  @tailrec
  def find(cs: List[Char], lc: Char = ' ', stack: List[Int] = List.empty): Set[Int] =
    cs match {
      case h :: rest =>
        if (lc == h) find(rest, h, (weight(h) + stack.head) :: stack) else find(rest, h, weight(h) :: stack)
      case Nil => stack.toSet
    }

  def weightedUniformStrings(s: String, queries: Array[Int]): Array[String] = {
    val list = find(s.toCharArray.toList)
    queries.map(x => if (list.contains(x)) "Yes" else "No")
  }
}
