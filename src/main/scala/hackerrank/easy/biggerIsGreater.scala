package hackerrank.easy

object biggerIsGreater {

  def biggerIsGreater(w: String): String = {
    def longest(cs: Array[Char], i: Int): Int          = if (i > 0 && cs(i - 1) >= cs(i)) longest(cs, i - 1) else i
    def greater(cs: Array[Char], c: Char, i: Int): Int = if (cs(i) >= c) i else greater(cs, c, i - 1)
    val cs                                             = w.toCharArray
    val pivot                                          = longest(cs, w.length - 1)

    if (pivot == 0) "no answer"
    else {
      val (fix, right) = cs.splitAt(pivot)
      val gt           = greater(right, fix(pivot - 1), right.length - 1)
      println(fix.mkString, right.mkString, gt)
      val tmp = fix(pivot - 1)
      fix(pivot - 1) = right(gt)
      right(gt) = tmp
      (fix ++ right.reverse).mkString
    }
  }
}
