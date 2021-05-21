package hackerrank.medium.dp

import scala.collection.immutable.Queue

// https://www.hackerrank.com/challenges/lego-blocks/problem
object LegoBlocks {

  def legoBlocks(n: Int, m: Int): Int = {
    BigInt(23).pow(13)
    1
  }
  Queue(1, 2).dequeue
}

object hackerrankInString {

  def find(s: List[Char], stack: List[Char]): String = (s, stack) match {
    case (a :: aa, b :: bb) => if (a == b) find(aa, bb) else find(aa, b :: bb)
    case (Nil, b :: bb)     => "NO"
    case (_, Nil)           => "YES"
  }
}
