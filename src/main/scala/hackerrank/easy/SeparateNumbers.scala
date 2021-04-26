package hackerrank.easy

object SeparateNumbers {

  def beautiful(curr: List[Char], next: Long): Boolean =
    if (curr.isEmpty) true
    else {
      val (c, r) = curr.splitAt(next.toString.length)
      if (c.mkString == next.toString) beautiful(r, next + 1) else false
    }

  def find(s: String, level: Int): String =
    if (level > s.length / 2 && level < 18) "NO"
    else {
      val (h, r) = s.splitAt(level)
      if (beautiful(r.toCharArray.toList, h.toLong + 1)) s"YES $h" else find(s, level + 1)
    }

  val str = "58589967442418995858996744241900"
  find(str, 1)
}
