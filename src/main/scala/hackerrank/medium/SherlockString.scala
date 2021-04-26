package hackerrank.medium

// https://www.hackerrank.com/challenges/sherlock-and-valid-string/problem
object SherlockString {

  def isValid(s: String): String = {
    val s1 = s.toCharArray.groupBy(identity).map { case (a, b) => (a, b.length) }
    val s2 = s1.toList.groupBy(_._2).map { case (a, b) => (a, b.length) }.toList.sortBy(_._1)

    if (s2.length == 1) "YES"
    else if (s2.length == 2) {
      val List((a1, b1), (a2, b2)) = s2
      val v                        = (a2 - a1 == 1) && (b1 == 1 || b2 == 1) || (a1 == 1 && b1 == 1)
      if (v) "YES" else "NO"
    } else "NO"
  }

}
