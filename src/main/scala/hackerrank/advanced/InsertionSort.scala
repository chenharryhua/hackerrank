package hackerrank.advanced

import better.files._

object InsertionSort {

  final class BIT(m: Map[Int, Int] = Map.empty) {
    private val Max: Int = 10000000

    def add(key: Int): BIT = {
      val rst = LazyList.iterate(key)(s => s + (s & (-s))).takeWhile(x => x < Max && x > 0).foldLeft(m) { case (s, i) =>
        s.updatedWith(i)(_.map(_ + 1).orElse(Some(1)))
      }
      new BIT(rst)
    }

    def get(key: Int): Long =
      LazyList.iterate(key)(s => s - (s & (-s))).takeWhile(_ > 0).foldLeft(0L) { case (s, i) => m.getOrElse(i, 0) + s }

    override def toString: String = m.toString
  }

  def insertionSort(arr: Array[Int]): Long =
    arr
      .foldLeft((new BIT(), 0L, 0)) { case ((tm, s, idx), v) =>
        val d = idx - tm.get(v)
        (tm.add(v), s + d, idx + 1)
      }
      ._2

  val arr = Array(12, 15, 1, 5, 6, 14, 11)

  println(insertionSort(file"/Users/chenh/Downloads/test.txt".contentAsString.split(" ").map(_.toInt)))

}

object mergeSort {

  def merge(arr: Array[Int], start: Int, mid: Int, end: Int): Long = {
    val left  = arr.slice(start, mid) :+ Int.MaxValue
    val right = arr.slice(mid, end + 1) :+ Int.MaxValue
    (start to end)
      .foldLeft((0, 0, 0L)) { case ((l, r, c), i) =>
        if (left(l) <= right(r)) {
          arr(i) = left(l)
          (l + 1, r, c)
        } else {
          arr(i) = right(r)
          (l, r + 1, c + mid + r - i)
        }
      }
      ._3
  }

  def mergeSort(arr: Array[Int], start: Int, end: Int): Long =
    if (start < end) {
      val mid    = (start + end) / 2
      val left   = mergeSort(arr, start, mid)
      val right  = mergeSort(arr, mid + 1, end)
      val merged = merge(arr, start, mid + 1, end)
      left + right + merged
    } else 0L

  val arr = Array(4, 3, 2, 1)
  println(mergeSort(arr, 0, arr.length - 1))
  println(arr.toList)
}
