package hackerrank.easy

object queensAttack {

  def queensAttack(n: Int, k: Int, r_q: Int, c_q: Int, obstacles: Array[Array[Int]]): Int = {

    val ob = obstacles.map(x => (x(0), x(1))).toSet

    def rightUp(r_q: Int, c_q: Int, count: Int): Int =
      if (r_q > n || c_q > n || ob.contains((r_q, c_q))) count else rightUp(r_q + 1, c_q + 1, count + 1)

    def rightDown(r_q: Int, c_q: Int, count: Int): Int =
      if (r_q < 1 || c_q < 1 || ob.contains((r_q, c_q))) count else rightDown(r_q - 1, c_q - 1, count + 1)

    def leftDown(r_q: Int, c_q: Int, count: Int): Int =
      if (r_q < 1 || c_q > n || ob.contains((r_q, c_q))) count else leftDown(r_q - 1, c_q + 1, count + 1)

    def leftUp(r_q: Int, c_q: Int, count: Int): Int =
      if (r_q > n || c_q < 1 || ob.contains((r_q, c_q))) count else leftUp(r_q + 1, c_q - 1, count + 1)

    def rowLeft(c_q: Int, count: Int): Int =
      if (c_q < 1 || ob.contains((r_q, c_q))) count else rowLeft(c_q - 1, count + 1)

    def rowRight(c_q: Int, count: Int): Int =
      if (c_q > n || ob.contains((r_q, c_q))) count else rowRight(c_q + 1, count + 1)

    def colUp(r_q: Int, count: Int): Int =
      if (r_q > n || ob.contains((r_q, c_q))) count else colUp(r_q + 1, count + 1)

    def colDown(r_q: Int, count: Int): Int =
      if (r_q < 1 || ob.contains((r_q, c_q))) count else colDown(r_q - 1, count + 1)

    val ru = rightUp(r_q + 1, c_q + 1, 0)
    val rd = rightDown(r_q - 1, c_q - 1, 0)
    val lu = leftUp(r_q + 1, c_q - 1, 0)
    val ld = leftDown(r_q - 1, c_q + 1, 0)
    val rl = rowLeft(c_q - 1, 0)
    val rr = rowRight(c_q + 1, 0)
    val cu = colUp(r_q + 1, 0)
    val cd = colDown(r_q - 1, 0)
    println("rightup" + ru)
    println("rightdown" + rd)
    println("leftup" + lu)
    println("leftdown" + ld)
    println("rowLeft" + rl)
    println("rowright" + rr)
    println("colup" + cu)
    println("coldown" + cd)

    ru + rd + lu + ld + rl + rr + cu + cd

  }
}
