package hackerrank.medium.search

// https://www.hackerrank.com/challenges/count-luck/problem
object CountLuck {

  final case class Node(
    row: Int,
    col: Int,
    up: Boolean,
    down: Boolean,
    left: Boolean,
    right: Boolean,
    wanded: Boolean) {
    val deadEnd: Boolean = !(up || down || left || right)
  }

  final class Board(board: Array[Array[Char]]) {
    private val rows = board.length
    private val cols = board(0).length

    def isOpen(row: Int, col: Int): Boolean =
      row >= 0 && col >= 0 && row < rows && col < cols &&
        (board(row)(col) == '.' || board(row)(col) == '*' || board(row)(col) == 'M')

    def node(row: Int, col: Int): Node = {
      val up    = isOpen(row + 1, col)
      val down  = isOpen(row - 1, col)
      val left  = isOpen(row, col - 1)
      val right = isOpen(row, col + 1)
      Node(row, col, up, down, left, right, List(up, down, left, right).count(identity) > 2)
    }
    def left(n: Node): Node  = node(n.row, n.col - 1).copy(right = false)
    def right(n: Node): Node = node(n.row, n.col + 1).copy(left = false)
    def up(n: Node): Node    = node(n.row + 1, n.col).copy(down = false)
    def down(n: Node): Node  = node(n.row - 1, n.col).copy(up = false)

    def bingo(n: Node): Boolean = n.row >= 0 && n.col >= 0 && n.row < rows && n.col < cols && board(n.row)(n.col) == '*'

  }

  def find(board: Board, curr: Node, stack: List[Node]): Option[List[Node]] =
    if (board.bingo(curr)) Some(stack)
    else if (curr.deadEnd && stack.nonEmpty) find(board, stack.head, stack.tail)
    else if (curr.deadEnd && stack.isEmpty) None
    else {
      if (curr.left) find(board, board.left(curr), curr.copy(left = false) :: stack)
      else if (curr.right) find(board, board.right(curr), curr.copy(right = false) :: stack)
      else if (curr.up) find(board, board.up(curr), curr.copy(up = false) :: stack)
      else if (curr.down) find(board, board.down(curr), curr.copy(down = false) :: stack)
      else None
    }

  def countLuck(matrix: Array[String], k: Int): String = {
    val b = new Board(matrix.map(_.toCharArray))
    val (row, col) = matrix.zipWithIndex.flatMap { case (s, idx) =>
      s.toCharArray.zipWithIndex.find(_._1 == 'M').map(x => (idx, x._2))
    }.head

    val node = b.node(row, col)

    find(b, node.copy(wanded = List(node.up, node.down, node.left, node.right).count(identity) > 1), List.empty)
      .fold("Oops!")(x => if (x.count(_.wanded) == k) "Impressed" else "Oops!")
  }
}
