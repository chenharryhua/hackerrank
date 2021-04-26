package hackerrank.medium.greedy

// https://www.hackerrank.com/challenges/pylons/problem
object GoodlandElectricity {

  def pylons(k: Int, arr: Array[Int]): Int = {
    val len = arr.length
    def compu(idx: Int, count: Int): Option[Int] =
      if (idx >= len) Some(count)
      else {
        val from = Math.max(idx - k + 1, 0)
        val to   = Math.min(idx + k, len)
        arr.slice(from, to).zipWithIndex.reverse.find(_._1 == 1).flatMap(c => compu(c._2 + from + k, count + 1))
      }

    compu(0, 0) match {
      case None      => -1
      case Some(ans) => ans
    }
  }

  println(pylons(2, Array(0, 1, 1, 1, 1, 0)))
}
