package hackerrank.medium.graph

object JeanisRoute {

  // https://www.hackerrank.com/challenges/jeanies-route/problem
  def diameter(edges: Array[Set[(Int, Int)]], nodes: Array[Boolean]): Long = {
    def bfs(pending: java.util.Queue[(Int, Long)]): (Int, Long) = {
      var maxNode: Int            = 0
      var maxWeight: Long         = 0
      val visited: Array[Boolean] = Array.fill(edges.length + 1)(false)
      while (!pending.isEmpty) {
        val (n, v) = pending.poll()
        if (!visited(n)) {
          if (nodes(n) && v > maxWeight) {
            maxNode = n
            maxWeight = v
          }
          edges(n).filterNot(x => visited(x._1)).foreach(x => pending.add((x._1, x._2 + v)))
          visited(n) = true
        }
      }
      (maxNode, maxWeight)
    }

    val adq   = new java.util.ArrayDeque[(Int, Long)]
    val first = nodes.zipWithIndex.dropWhile(!_._1).head._2
    adq.add((first, 0))
    val second = bfs(adq)._1
    adq.add((second, 0))
    bfs(adq)._2
  }

  def height(edges: Array[Set[(Int, Int)]], nodes: Array[Boolean]): Long = {
    def dfs(visited: Array[Boolean], curr: Int, weight: Long): (Long, Boolean) = {
      val next = edges(curr)
      if (next.isEmpty) (weight, nodes(curr))
      else {
        next.foldLeft((weight, nodes(curr))) { case ((s, b), (n, w)) =>
          if (visited(n)) (s, b)
          else {
            visited(n) = true
            val (lw, lb) = dfs(visited, n, w)
            val sum      = if (lb) s + lw else s
            val flag     = lb || b
            (sum, flag)
          }
        }
      }
    }
    val v     = Array.fill(edges.length + 1)(false)
    val first = nodes.zipWithIndex.dropWhile(!_._1).head._2
    v(first) = true
    dfs(v, first, 0)._1
  }

  def fmain(args: Array[String]): Unit = {
    val sc                   = new java.util.Scanner(System.in)
    val n                    = sc.nextInt
    val k                    = sc.nextInt
    val city: Array[Boolean] = Array.fill(k + 1)(false)
    for (i <- 0 until k) city(sc.nextInt) = true
    val roads = Array.fill(n + 1)(Set.empty[(Int, Int)])
    for (i <- 0 until n - 1) {
      val f = sc.nextInt()
      val t = sc.nextInt()
      val w = sc.nextInt()
      roads(f) += (t -> w)
      roads(t) += (f -> w)
    }
    val sum = height(roads, city)
    val M   = diameter(roads, city)
    println((sum * 2 - M))
    sc.close
  }

}
