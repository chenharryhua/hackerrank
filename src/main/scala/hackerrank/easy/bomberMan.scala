package hackerrank.easy

object bomberMan {

  final case class Grid(board: Array[Array[Int]], rows: Int, cols: Int) {
    def plant: Grid  = Grid(board.map(_.map(x => if (x == 0) 3 else x - 1)), rows, cols)
    def wait1s: Grid = Grid(board.map(_.map(x => if (x == 3) 2 else x)), rows, cols)

    def explode: Grid = {
      val g = Array.fill(rows)(Array.fill(cols)(0))
      for {
        r <- 0 until rows
        c <- 0 until cols
      } if (!(hasBomb(r, c) || hasBomb(r - 1, c) || hasBomb(r + 1, c) || hasBomb(r, c - 1) || hasBomb(r, c + 1)))
        g(r)(c) = board(r)(c) - 1
      Grid(g, rows, cols)
    }

    private def hasBomb(r: Int, c: Int): Boolean =
      if (r >= 0 && r < rows && c >= 0 && c < cols) board(r)(c) == 1 else false

    def result: Array[String] = board.map(_.map(x => if (x > 0) "O" else ".").mkString)

  }

  def bomberMan(n: Int, grid: Array[String]): Array[String] = {
    val rows = grid.length
    val cols = grid(0).length
    val init = Grid(grid.map(s => s.map(x => if (x == 'O') 3 else 0).toArray), rows, cols)

    if (n == 1) init.result
    else if (n == 2) init.wait1s.plant.result
    else if ((n - 3) % 4 == 0) init.wait1s.plant.explode.result
    else if ((n - 3) % 4 == 2) init.wait1s.plant.explode.plant.explode.result
    else init.plant.result
  }

}
