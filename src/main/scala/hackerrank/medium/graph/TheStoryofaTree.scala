package hackerrank.medium.graph

// https://www.hackerrank.com/challenges/the-story-of-a-tree/problem
object TheStoryofaTree {

  @scala.annotation.tailrec
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  final case class Node(parent: Int, self: Int, children: Set[Int])
  final case class Guess(children: Int, parent: Int)

  def directed(graph: Map[Int, Set[Int]], curr: Int, prev: Int): Set[Node] = {
    val c = graph(curr)
    val n = c.foldLeft(Set.empty[Node]) { case (s, i) =>
      if (i == prev) s
      else s ++ directed(graph, i, curr)
    }
    n + Node(prev, curr, c - prev)
  }

  def bingo(tree: Map[Int, Set[Int]], k: Int, guess: List[Guess]): Boolean =
    guess.map(g => tree(g.children).contains(g.parent)).count(identity) >= k

  def dfs(k: Int, guess: List[Guess], tree: Map[Int, Set[Int]], curr: Int, prev: Int): (Int, Map[Int, Set[Int]]) = {
    val childrens = tree(curr)
    if (childrens.isEmpty) {
      val nt = tree.updatedWith(prev)(_.map(_ - curr)).updatedWith(curr)(_.map(_ + prev).orElse(Some(Set(prev))))
      (if (bingo(nt, k, guess)) 1 else 0, nt)
    } else {
      val (nc, nt) = childrens.foldLeft((0, tree)) { case ((acc, t), n) =>
        val (r, nt) = dfs(k, guess, t, n, curr)
        (acc + r, nt)
      }
      val nt2 = nt.updatedWith(prev)(_.map(_ - curr)).updatedWith(curr)(_.map(_ + prev).orElse(Some(Set(prev))))
      (if (bingo(nt2, k, guess)) nc + 1 else nc, nt2)
    }
  }

  def storyOfATree(n: Int, edges: Array[Array[Int]], k: Int, guesses: Array[Array[Int]]): String = {
    val graph = edges.flatMap { case Array(f, t) => Array((f, t), (t, f)) }.groupBy(_._1).map { case (f, es) =>
      f -> es.map(_._2).toSet
    }
    val guess: List[Guess] = guesses.map { case Array(c, p) => Guess(c, p) }.toList
    val tree: Map[Int, Set[Int]] =
      directed(graph, 1, -1).groupBy(_.self).map { case (n, s) => n -> s.flatMap(_.children) }
    val ans = dfs(k, guess, tree, 1, -1)
    val res = ans._1
    val g   = gcd(res, n)
    println(tree)
    println(ans)
    s"${res / g}/${n / g}"
  }

  val arr = Array(
    Array(94, 83),
    Array(46, 81),
    Array(14, 86),
    Array(46, 79),
    Array(60, 14),
    Array(41, 71),
    Array(54, 41),
    Array(38, 19),
    Array(89, 60),
    Array(56, 67),
    Array(81, 21),
    Array(76, 54),
    Array(66, 64),
    Array(90, 3),
    Array(32, 33),
    Array(83, 8),
    Array(78, 50),
    Array(91, 39),
    Array(40, 81),
    Array(54, 72),
    Array(30, 73),
    Array(71, 13),
    Array(43, 32),
    Array(15, 95),
    Array(98, 29),
    Array(57, 31),
    Array(22, 33),
    Array(73, 1),
    Array(49, 91),
    Array(83, 35),
    Array(49, 59),
    Array(78, 88),
    Array(80, 6),
    Array(94, 38),
    Array(8, 56),
    Array(11, 45),
    Array(41, 50),
    Array(96, 78),
    Array(45, 53),
    Array(17, 57),
    Array(63, 9),
    Array(57, 3),
    Array(68, 98),
    Array(28, 92),
    Array(61, 70),
    Array(18, 26),
    Array(56, 33),
    Array(14, 61),
    Array(54, 44),
    Array(97, 33),
    Array(80, 63),
    Array(37, 42),
    Array(17, 76),
    Array(80, 99),
    Array(7, 57),
    Array(37, 35),
    Array(65, 9),
    Array(16, 10),
    Array(41, 99),
    Array(30, 84),
    Array(2, 72),
    Array(87, 64),
    Array(32, 15),
    Array(19, 7),
    Array(86, 4),
    Array(25, 41),
    Array(58, 48),
    Array(50, 23),
    Array(11, 72),
    Array(38, 30),
    Array(69, 39),
    Array(67, 46),
    Array(49, 23),
    Array(12, 89),
    Array(30, 66),
    Array(52, 51),
    Array(5, 45),
    Array(89, 27),
    Array(25, 4),
    Array(75, 28),
    Array(93, 68),
    Array(97, 16),
    Array(10, 20),
    Array(40, 85),
    Array(24, 100),
    Array(7, 68),
    Array(28, 56),
    Array(90, 100),
    Array(58, 47),
    Array(55, 81),
    Array(6, 51),
    Array(47, 76),
    Array(98, 77),
    Array(82, 62),
    Array(47, 36),
    Array(67, 26),
    Array(62, 17),
    Array(27, 74),
    Array(56, 34)
  )

  val guess = Array(
    Array(14, 60),
    Array(90, 3),
    Array(67, 26),
    Array(46, 81),
    Array(76, 47),
    Array(57, 3),
    Array(47, 58),
    Array(93, 68),
    Array(58, 48),
    Array(6, 80),
    Array(88, 78),
    Array(41, 25),
    Array(33, 32),
    Array(40, 85),
    Array(80, 6),
    Array(61, 70),
    Array(4, 86),
    Array(15, 32),
    Array(68, 93),
    Array(27, 74),
    Array(98, 77),
    Array(79, 46),
    Array(32, 43),
    Array(24, 100),
    Array(83, 35),
    Array(78, 88),
    Array(60, 89),
    Array(83, 94),
    Array(25, 4),
    Array(66, 30),
    Array(49, 59),
    Array(49, 23),
    Array(7, 68),
    Array(77, 98),
    Array(47, 36),
    Array(22, 33),
    Array(98, 68),
    Array(100, 90),
    Array(35, 83),
    Array(16, 97),
    Array(74, 27),
    Array(86, 4),
    Array(69, 39),
    Array(38, 94),
    Array(61, 14),
    Array(53, 45),
    Array(26, 18),
    Array(56, 67),
    Array(19, 7),
    Array(57, 17),
    Array(41, 50),
    Array(97, 16),
    Array(23, 50),
    Array(81, 21),
    Array(10, 16),
    Array(7, 19),
    Array(73, 30),
    Array(37, 35),
    Array(87, 64),
    Array(67, 56),
    Array(29, 98),
    Array(40, 81),
    Array(25, 41),
    Array(11, 72),
    Array(65, 9),
    Array(54, 72),
    Array(45, 5),
    Array(6, 51),
    Array(41, 54),
    Array(94, 83),
    Array(42, 37),
    Array(30, 66),
    Array(3, 90),
    Array(12, 89),
    Array(64, 87),
    Array(17, 76),
    Array(72, 54),
    Array(37, 42),
    Array(71, 13),
    Array(35, 37),
    Array(28, 92),
    Array(54, 76),
    Array(68, 98),
    Array(21, 81),
    Array(39, 91),
    Array(90, 100),
    Array(82, 62),
    Array(81, 55),
    Array(1, 73),
    Array(15, 95),
    Array(58, 47),
    Array(70, 61),
    Array(86, 14),
    Array(85, 40),
    Array(39, 69),
    Array(44, 54),
    Array(45, 11),
    Array(83, 8),
    Array(78, 50)
  )
  println(storyOfATree(100, arr, 50, guess))
}
