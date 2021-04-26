package hackerrank.easy

object BreakingRecord {

  def breakingRecords(scores: Array[Int]): Array[Int] = {
    val break = scores
      .scan(0) { case (s, i) => if (i > s) i else s }
      .tail
      .sliding(2)
      .map {
        case Array(a, b) => if (b > a) true else false
        case _           => false
      }
      .count(identity)

    val low = scores
      .scan(Int.MaxValue) { case (s, i) => if (i < s) i else s }
      .tail
      .sliding(2)
      .map {
        case Array(a, b) =>
          if (b < a) true else false
        case _ => false
      }
      .count(identity)

    Array(break, low)
  }
}
