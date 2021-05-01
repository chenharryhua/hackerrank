package hackerrank.medium.bit

import better.files._

// https://www.hackerrank.com/challenges/winning-lottery-ticket/problem
object WinningLotteryTicket extends App {

  def winningLotteryTicket(tickets: Array[String]): Long = {
    val frequency = Array.ofDim[Long](1024)
    tickets.foreach { s =>
      val idx = s.toCharArray.foldLeft(0) { case (s, c) =>
        s | (1 << (c - 48))
      }
      frequency(idx) += 1
    }
    val count = (0 until 1023)
      .filterNot(frequency(_) == 0)
      .flatMap { i =>
        (i + 1 until 1024).map { j =>
          if ((i | j) == 1023) frequency(i) * frequency(j) else 0
        }
      }
      .sum
    count + frequency(1023) * (frequency(1023) - 1) / 2
  }

  val r1 = file"/Users/chenh/Downloads/input01.txt".lines.toArray
  println(winningLotteryTicket(r1))
}
