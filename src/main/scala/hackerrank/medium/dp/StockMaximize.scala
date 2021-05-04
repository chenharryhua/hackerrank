package hackerrank.medium.dp

//https://www.hackerrank.com/challenges/stockmax/problem
object StockMaximize {

  def stockmax(prices: Array[Int]): Long =
    prices
      .foldRight((0L, 0L)) { case (i, (s, t)) =>
        if (i > t) (s, i)
        else (s + t - i, t)
      }
      ._1

  println(stockmax(Array(1, 2, 100)))
}
