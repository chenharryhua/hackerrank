package hackerrank.easy

object substrCount {

  def substrCount(n: Int, s: String): Long = {
    val cs  = s.toCharArray
    val len = s.length
    val single = cs
      .foldLeft(List((Char.MinValue, 0))) { case ((c, s) :: rest, i) =>
        if (c == i) (c, s + 1) :: rest else (i, 1) :: (c, s) :: rest
      }
      .map(x => (x._2 - 1) * x._2 / 2)
      .sum

    def expand(idx: Int, c: Char, count: Int): Int = {
      val right = idx + 2 * (count + 1)
      if (idx >= 0 && right < len && cs(idx) == cs(right) && cs(idx) == c) expand(idx - 1, c, count + 1)
      else count
    }

    val mvm = cs.toList
      .sliding(3)
      .zipWithIndex
      .map { case (List(a, b, c), idx) => if (a == c && a != b) expand(idx, a, 0) else 0 }
      .sum

    single + mvm + s.length
  }
}
