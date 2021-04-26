package hackerrank.easy

import java.math.MathContext

object encryption {

  def encryption(s: String): String = {
    val noSpace = s.filterNot(_.isSpaceChar)
    val l       = noSpace.length

    val c = Math.ceil(Math.pow(l.toDouble, 1.0 / 2))
    val f = Math.floor(Math.pow(l.toDouble, 1.0 / 2))

    val ca = Math.ceil(l / c) * c
    val fa = Math.ceil(l / f) * f

    def take(ss: String, n: Int, stack: List[String]): List[String] = {
      val ns = " " * (n - ss.take(n).length)
      if (ss.isEmpty) stack.reverse else take(ss.drop(n), n, (ss.take(n) + ns) :: stack)
    }

    val n = (if (fa < l) f else c).toInt

    val arr = take(s, n, List.empty).map(_.toCharArray)
    val m   = arr.length
    val rst = for {
      i <- 0 until n
    } yield (0 until m).map(x => arr(x)(i)).mkString.filterNot(_.isSpaceChar)
    rst.mkString(" ")

  }
}
