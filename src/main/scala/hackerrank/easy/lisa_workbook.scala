package hackerrank.easy

//https://www.hackerrank.com/challenges/lisa-workbook/problem?isFullScreen=true
class lisa_workbook {

  val arr = Array(4, 2, 6, 1, 10)

  private def pagination(p: Int): List[List[Int]] =
    List.range(1, p).sliding(3).toList

  arr
    .foldLeft(List.empty[List[Int]]) { case (sum, item) =>
      sum ::: pagination(item)
    }
    .zipWithIndex
    .count { case (lst, i) => lst.contains(i + 1) }


}
