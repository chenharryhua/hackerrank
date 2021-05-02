package hackerrank.medium.bit

import scala.collection.immutable.BitSet
import better.files._

// https://www.hackerrank.com/challenges/yet-another-minimax-problem/problem
object YetAnotherMinimaxProblem {

  def anotherMinimaxProblem(a: Array[Int]): Long = {
    val len    = a.length
    val bs     = a.map(x => BitSet.fromBitMask(Array(x)))
    val allOne = bs.flatten.groupBy(identity).filter { case (_, arr) => (arr.length == len) }.keySet
    val bbs    = bs.map(_ -- allOne).toSet
    bbs.flatMap(_.maxOption).maxOption match {
      case Some(lm) =>
        val (p1, p2) = bbs.partition(_.maxOption.contains(lm))
        val ans = for {
          i <- p1
          j <- p2
        } yield (i ^ j).toBitMask(0)
        ans.min
      case None => 0L
    }
  }

  val r1 = file"/Users/chenh/Downloads/input22.txt".lines.flatMap(_.split(" ").map(_.toInt)).toArray
  println(anotherMinimaxProblem(r1))
  println(anotherMinimaxProblem(Array(0, 0, 0, 0)))
}

object YetAnotherMinimaxProblemBruteForce {

  def anotherMinimaxProblem(a: Array[Int]): Long =
    a.permutations.map(_.sliding(2).map(p => p(0) ^ p(1)).max).min
}
