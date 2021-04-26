package hackerrank.medium

object SuperDigital {

  def superDigit(n: String, k: Int): Int = {
    def go(s: String): Int =
      s.toList.map(_.toString.toInt) match {
        case a :: Nil => a
        case all      => go(all.sum.toString)
      }
    go((go(n) * k).toString)
  }
  println(
    superDigit(
      "3546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736",
      100000))
}
