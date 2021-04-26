package hackerrank.easy

object ClimbingLeaderboard {

  def climbingLeaderboard(ranked: Array[Int], player: Array[Int]): Array[Int] = {
    val ranking = (ranked
      .appended(0))
      .scanLeft((0, Int.MaxValue, Int.MaxValue)) { case ((r, s, e), i) => if (i == e) (r, s, e) else (r + 1, e, i) }
      .tail
      .toList

    def find(rk: List[(Int, Int, Int)], p: Int): (List[(Int, Int, Int)], Int) =
      rk match {
        case (r, s, e) :: rest => if (p >= e && p < s) ((r, s, e) :: rest, r) else find(rest, p)
      }
    player.scanRight((ranking, 0)) { case (p, (rs, r)) => find(rs, p) }.map(_._2).dropRight(1)
  }

  val ranked = Array(100, 100, 50, 40, 40, 20, 10)
  val player = Array(5, 25, 50, 120)

  climbingLeaderboard(ranked, player)

  def designerPdfViewer(h: Array[Int], word: String): Int = {
    val ascii = "abcdefghijklmnopqrstuvwxyz"
    val map   = ascii.toCharArray.zip(h).toMap
    word.toCharArray.map(x => map(x)).max * word.length

  }

  def utopianTree(n: Int): Int =
    (0 until n).scanLeft((1, true)) { case ((s, c), i) => if (c) (s * 2, false) else (s + 1, true) }.map(_._1).last

  def angryProfessor(k: Int, a: Array[Int]): String =
    if (a.count(_ <= 0) >= k) "NO" else "YES"

  def beautifulDays(i: Int, j: Int, k: Int): Int =
    (i to j).map(x => (x - x.toString.reverse.toInt) % k == 0).count(identity)

  def permutationEquation(p: Array[Int]): Array[Int] = {
    val rst = for {
      x <- (1 to p.length)
      y <- p
    } yield if (p(p(y - 1) - 1) == x) Some(y) else None
    rst.flatten.toArray
  }

}
