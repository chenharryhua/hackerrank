package hackerrank.easy

object PageCount {

  def pageCount(n: Int, p: Int): Int = {
    val book = LazyList.from(0, 2).map(x => (x, x + 1)).takeWhile { case (a, b) => !(a > n && b > n) }.toList

    def find(bk: List[(Int, Int)], stack: Int): Int = bk match {
      case (a, b) :: rest => if (a == p || b == p) stack else find(rest, stack + 1)
    }
    Math.min(find(book, 0), find(book.reverse, 0))
  }
}
