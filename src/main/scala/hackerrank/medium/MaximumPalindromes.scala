package hackerrank.medium

import better.files._

// https://www.hackerrank.com/challenges/maximum-palindromes/problem
object MaximumPalindromes {
  val Mod = 1000000007L

  final case class CharFreq(s: String) {
    private val em = Map.empty[Char, Int]

    private val freq: Vector[Map[Char, Int]] = s.toCharArray
      .foldLeft((Vector(em), em)) { case ((v, m), c) =>
        val nm = m.updatedWith(c)(_.map(_ + 1).orElse(Some(1)))
        (v :+ nm, nm)
      }
      ._1

    def charNum(f: Int, t: Int): Map[Char, Int] = {
      val m = freq(f - 1)
      freq(t).flatMap { case (c, n) =>
        val number = n - m.getOrElse(c, 0)
        if (number > 0) Some(c, number) else None
      }
    }
  }

  final case class Factorial(max: Int) {

    val (fact, inverse, _) = (1 until max).foldLeft((Vector(1L), Vector(1L), 1L)) { case ((v, iv, last), i) =>
      val f = (last * i) % Mod
      val p = BigInt(f).modPow(Mod - 2, Mod).toLong
      (v :+ f, iv :+ p, f)
    }
  }

  var cf: CharFreq    = null
  val fact: Factorial = Factorial(100100)

  def initialize(s: String): Unit = {
    cf = CharFreq(s)
    ()
  }

  def answerQuery(l: Int, r: Int): Long = {
    val ss      = cf.charNum(l, r)
    val singles = ss.count(_._2 % 2 == 1)
    val sides   = ss.map(_._2 / 2).sum
    val f       = fact.fact(sides)
    val inv     = fact.inverse
    val is      = ss.values.foldLeft(BigInt(1)) { case (s, i) => (s * inv(i / 2)) % Mod }
    if (singles == 0) ((f * is) % Mod).toLong else ((singles * f * is) % Mod).toLong
  }

  initialize(file"/Users/chenh/Downloads/test.txt".contentAsString)
  //(initialize("daadabbadcabacbcccbdcccdbcccbbaadcbabbdaaaabbbdabdbbdcadaaacaadadacddabbbbbdcccbaabbbacacddbbbcbbdbd"))
  println(answerQuery(148, 99940))
  println(answerQuery(10, 99785))
  println(answerQuery(164, 99823))
  println(answerQuery(233, 99745))
}
