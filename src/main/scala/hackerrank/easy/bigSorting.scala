package hackerrank.easy

object bigSorting {

  implicit def stringOrdering: Ordering[String] = new Ordering[String] {

    override def compare(x: String, y: String): Int = if (x.length != y.length) x.length - y.length
    else {
      x.zip(y).find { case (a, b) => (a != b) }.map { case (a, b) => a - b }.getOrElse(0)
    }
  }

  def bigSorting(unsorted: Array[String]): Array[String] =
    unsorted.sorted

  def introTutorial(V: Int, arr: Array[Int]): Int =
    arr.zipWithIndex.find(_._1 == V).map(_._2 + 1).getOrElse(0)

  def insertionSort1(n: Int, arr: Array[Int]): Unit = {
    def right(a: Array[Int], v: Int, idx: Int): Unit =
      if (idx >= 0 && a(idx) > v) {
        a(idx + 1) = a(idx)
        println(a.mkString(" "))
        right(a, v, idx - 1)
      } else {
        a(idx + 1) = v
        println(a.mkString(" "))
      }

    right(arr, arr(n - 1), n - 2)
  }

  def insertionSort2(n: Int, arr: Array[Int]) = {
    def sort(left: List[Int], right: List[Int], count: Int): Int =
      right match {
        case h :: rest =>
          val l = (h :: left).sorted
          if (l ::: rest != left ::: right) {
            val c = l.length - l.zip(left).takeWhile { case (a, b) => a == b }.length - 1
            sort(l, rest, count + c)
          } else sort(l, rest, count)
        case _ => count
      }

    val l = arr.toList

    if (l == l.sorted) 0 else sort(List(), l, 0)
  }

  def quickSort(arr: Array[Int]): Array[Int] = {
    val p = arr(0)
    val l = arr.partition(_ < p)._1
    val r = arr.partition(_ > p)._1
    l ++ Array(p) ++ r

  }

  def countingSort(arr: Array[Int]): Array[Int] = {
    val map = arr.groupBy(identity).map { case (a, b) => (a, b.length) }
    (0 until 100).map(x => map.getOrElse(x, 0)).toArray
  }

  def countSort(arr: Array[Array[String]]): Unit = {
    val tuple           = arr.map(x => (x(0).toInt, x(1)))
    val (first, second) = tuple.splitAt(arr.length / 2)
    val df              = first.map(x => (x._1, "-"))
    (df ++ second).sortBy(_._1).map(_._2).mkString(" ")

  }

  def closestNumbers(arr: Array[Int]): Array[Int] =
    arr.sorted
      .sliding(2)
      .map(x => (Math.abs(x(1) - x(0)), x(0), x(1)))
      .toList
      .groupBy(_._1)
      .toList
      .minBy(_._1)
      ._2
      .flatMap(x => List(x._2, x._3))
      .toArray

}
