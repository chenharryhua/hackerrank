package hackerrank.easy

object GridChallenge {

  def gridChallenge(grid: Array[String]): String = {
    val arr  = grid.map(_.toCharArray.sorted)
    val cols = arr(0).length
    val rows = arr.length
    val ans = (0 until cols).forall { i =>
      (0 until rows).map(j => arr(j)(i)).toList.sliding(2).forall {
        case List(a, b) => a <= b
        case _          => true
      }
    }
    if (ans) "YES" else "NO"
  }
}
