package hackerrank.easy

object ThreeDSurfaceArea {

  def formingMagicSquare(s: Array[Array[Int]]): Int = {
    val m1 = Array(
      Array(8, 3, 4),
      Array(1, 5, 9),
      Array(6, 7, 2)
    ).flatten
    val m2 = Array(
      Array(6, 1, 8),
      Array(7, 5, 3),
      Array(2, 9, 4)
    ).flatten
    val m3 = Array(
      Array(2, 7, 6),
      Array(9, 5, 1),
      Array(4, 3, 8)
    ).flatten
    val m4 = Array(
      Array(4, 9, 2),
      Array(3, 5, 7),
      Array(8, 1, 6)
    ).flatten
    val m5 = Array(
      Array(8, 1, 6),
      Array(3, 5, 7),
      Array(4, 9, 2)
    ).flatten
    val m6 = Array(
      Array(2, 9, 4),
      Array(7, 5, 3),
      Array(6, 1, 8)
    ).flatten
    val m7 = Array(
      Array(4, 3, 8),
      Array(9, 5, 1),
      Array(2, 7, 6)
    ).flatten
    val m8 = Array(
      Array(6, 7, 2),
      Array(1, 5, 9),
      Array(8, 3, 4)
    ).flatten

    val c1 = s.flatten.zip(m1).map { case (a, b) => Math.abs(a - b) }.sum
    val c2 = s.flatten.zip(m2).map { case (a, b) => Math.abs(a - b) }.sum
    val c3 = s.flatten.zip(m3).map { case (a, b) => Math.abs(a - b) }.sum
    val c4 = s.flatten.zip(m4).map { case (a, b) => Math.abs(a - b) }.sum
    val c5 = s.flatten.zip(m5).map { case (a, b) => Math.abs(a - b) }.sum
    val c6 = s.flatten.zip(m6).map { case (a, b) => Math.abs(a - b) }.sum
    val c7 = s.flatten.zip(m7).map { case (a, b) => Math.abs(a - b) }.sum
    val c8 = s.flatten.zip(m8).map { case (a, b) => Math.abs(a - b) }.sum
    List(c1, c2, c3, c4, c5, c6, c7, c8).min

  }

  def countingValleys(steps: Int, path: String): Int =
    path.toCharArray
      .map(x => if (x == 'D') -1 else 1)
      .scanLeft((0, false)) { case ((level, sw), dir) =>
        (level + dir, level == 0 && dir < 0)
      }
      .count(_._2)

  def getMoneySpent(keyboards: Array[Int], drives: Array[Int], b: Int): Int =
    keyboards.flatMap(kb => drives.map(_ + kb)).filter(_ <= b).maxOption.getOrElse(-1)

  def catAndMouse(x: Int, y: Int, z: Int): String = {
    val ca = Math.abs(x - z)
    val cb = Math.abs(y - z)
    if (ca < cb) "Cat A" else if (ca > cb) "Cat B" else "Mouse C"
  }

  def pickingNumbers(a: Array[Int]): Int = {
    val map = a.groupBy(identity).map { case (k, as) => (k, as.length) }
    (1 to 100).map(x => map.getOrElse(x, 0)).toList.sliding(2).map { case List(a, b) => a + b }.max

  }
}
