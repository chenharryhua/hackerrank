package hackerrank.medium.dp

// https://www.hackerrank.com/challenges/fair-cut/problem
object FairCut extends App {

  def fairCut(k: Int, arr: Array[Int]): Long = {
    val len    = arr.length
    val kn     = if (k * 2 < len) k else len - k
    val is     = arr.indices.toSet
    val st     = (len - kn * 2) / 2
    val li     = (0 until kn).map(_ * 2 + st).toSet
    val lu     = (is -- li).toList
    val sorted = arr.sorted.map(_.toLong)
    li.toList.flatMap(ii => lu.map(jj => Math.abs(sorted(ii) - sorted(jj)))).sum
  }
  println(
    fairCut(
      11,
      Array(691259308, 801371251, 345390019, 162749471, 998969126, 308205008, 430442891, 404642721, 532566673,
        266540863, 702197285, 749105392, 775025448, 20453591, 582291534, 132855413, 747557193, 129094259, 474372133,
        788391070)
    ))
}

object FairCutBF {

  def fairCut(k: Int, arr: Array[Int]): Long = {
    val is = arr.indices.toSet
    is.toList.combinations(k).foldLeft(Long.MaxValue) { case (s, i) =>
      val c   = (is -- i).toList
      val res = i.flatMap(ii => c.map(jj => Math.abs(arr(ii).toLong - arr(jj)))).sum
      if (res < s) res else s
    }
  }
}
