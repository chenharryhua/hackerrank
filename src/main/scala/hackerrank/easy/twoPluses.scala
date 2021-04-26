package hackerrank.easy

object twoPluses {

  final case class Cross(r: Int, c: Int, count: Int) {

    val set: Set[(Int, Int)] =
      (1 until count).toSet[Int].flatMap(x => Set((r + x, c), (r - x, c), (r, c + x), (r, c - x))) ++ Set((r, c))
    def overlap(o: Cross): Boolean = set.intersect(o.set).nonEmpty
    def result                     = (count - 1) * 4 + 1
  }

  def twoPluses(grid: Array[String]): Int = {
    val cs   = grid.map(_.toCharArray)
    val rows = cs.length
    val cols = cs(0).length
    val good = 'G'

    def expand(r: Int, c: Int, count: Int): Int = {
      val up    = r - count >= 0 && cs(r - count)(c) == good
      val down  = r + count < rows && cs(r + count)(c) == good
      val left  = c - count >= 0 && cs(r)(c - count) == good
      val right = c + count < cols && cs(r)(c + count) == good
      if (up && down && left && right) expand(r, c, count + 1) else count
    }

    val centers = (for {
      i <- 0 until rows
      j <- 0 until cols if cs(i)(j) == good
    } yield Cross(i, j, expand(i, j, 1))).toList
      .flatMap(x => (1 to x.count).map(y => Cross(x.r, x.c, y)))
      .sortBy(_.count)
      .reverse

    def find(ps: List[Cross], head: Cross): Option[Cross] =
      ps match {
        case h :: rest => if (head.overlap(h)) find(rest, head) else Some(h)
        case Nil       => None
      }

    centers.foldLeft(0) { case (s, i) =>
      if (find(centers, i).map(_.result * i.result).getOrElse(0) > s) println(i)
      Math.max(s, find(centers, i).map(_.result * i.result).getOrElse(0))
    }
  }

}
