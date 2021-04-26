package hackerrank.easy

object PokerNim {

  def pokerNim(k: Int, c: Array[Int]): String = {
    val ans = c.foldLeft(0) { case (s, i) => s ^ i }
    if (ans > 0) "First" else "Second"
  }

  def misereNim(s: Array[Int]): String = {
    val (xor, sum) = s.foldLeft((0, 0)) { case ((xr, s), i) => (xr ^ i, s + i) }
    val len        = s.length
    if (len % 2 == 0) {
      if (len != sum && xor == 0) "Second" else "First"
    } else if (len == sum || xor == 0) "Second"
    else "First"
  }

  def nimGame(pile: Array[Int]): String = {
    val ans = pile.foldLeft(0) { case (s, i) => s ^ i }
    if (ans == 0) "Second" else "First"
  }
}
