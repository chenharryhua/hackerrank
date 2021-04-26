package hackerrank.medium

// https://www.hackerrank.com/challenges/richie-rich/problem
object HighestValuePalindrome {
  final case class Status(idx: Int, value: Char, updated: Boolean)

  def highestValuePalindrome(s: String, n: Int, k: Int): String = {
    if (s.length == 1 && k > 0) return "9" else if (s.length == 1) return s

    val len = s.length
    val (left, right) = (0 until len / 2).foldLeft((List.empty[Status], List.empty[Status])) {
      case ((left, right), i) =>
        val r = len - i - 1
        if (s(i) == s(r)) {
          (Status(i, s(i), false) :: left, Status(r, s(r), false) :: right)
        } else if (s(i) > s(r)) {
          (Status(i, s(i), false) :: left, Status(r, s(i), true) :: right)
        } else
          (Status(i, s(r), true) :: left, Status(r, s(r), false) :: right)
    }
    val center: Option[Status] = if (len % 2 == 1) Some(Status(len / 2, s(len / 2), false)) else None

    val remainK = k - (left ++ right).map(_.updated).count(identity)

    if (remainK < 0) "-1"
    else if (remainK == 0) (left ::: right ::: center.toList).sortBy(_.idx).map(_.value).mkString
    else {
      val (l, r, rm) = left.reverse.zip(right.reverse).foldLeft((List.empty[Status], List.empty[Status], remainK)) {
        case ((nl, nr, rm), (l, r)) =>
          if (rm == 0) (l :: nl, r :: nr, rm)
          else if ((l.updated || r.updated) && l.value != '9')
            (l.copy(value = '9') :: nl, r.copy(value = '9') :: nr, rm - 1)
          else if (rm >= 2 && (!l.updated && !r.updated) && l.value != '9')
            (l.copy(value = '9') :: nl, r.copy(value = '9') :: nr, rm - 2)
          else (l :: nl, r :: nr, rm)
      }
      val c = if (rm > 0) center.map(_.copy(value = '9')).toList else center.toList
      (l ::: c ::: r).sortBy(_.idx).map(_.value).mkString
    }
  }
}
