package hackerrank.medium.greedy

import better.files._

import java.util.UUID

// https://www.hackerrank.com/challenges/cloudy-day/problem
object CloudyDay {
  sealed trait Position extends Product { def pos: Long }
  final case class City(pos: Long, population: Long) extends Position
  final case class Cloud(pos: Long, id: UUID) extends Position

  def maximumPeople(p: Array[Long], x: Array[Long], y: Array[Long], r: Array[Long]): Long = {
    val cities = x.zip(p).groupBy(_._1).map { case (p, n) => City(p, n.map(_._2).sum) }.toArray
    val clouds = y.zip(r).flatMap { case (p, r) =>
      val uuid = UUID.randomUUID()
      Array(Cloud(p - r - 1, uuid), Cloud(p + r, uuid))
    }
    val sunnyUUID = UUID.randomUUID()
    val (ans, _) = (cities ++ clouds).sortBy(_.pos).foldLeft((Map.empty[UUID, Long], Set.empty[UUID])) {
      case ((sum, cloudy), position) =>
        position match {
          case City(pos, population) =>
            val update =
              if (cloudy.isEmpty) sum.updatedWith(sunnyUUID)(_.map(_ + population).orElse(Some(population)))
              else if (cloudy.size == 1) sum.updatedWith(cloudy.head)(_.map(_ + population).orElse(Some(population)))
              else sum
            (update, cloudy)
          case Cloud(pos, id) => if (cloudy.contains(id)) (sum, cloudy - id) else (sum, cloudy + id)
        }
    }
    val s: Long = ans.getOrElse(sunnyUUID, 0)
    val c: Long = (ans - sunnyUUID).values.toList.sorted.lastOption.getOrElse(0L)
    s + c
  }

  val population = file"/Users/chenh/Downloads/population.txt".lines.flatMap(_.split(" ").map(_.toLong)).toArray
  val position   = file"/Users/chenh/Downloads/position.txt".lines.flatMap(_.split(" ").map(_.toLong)).toArray
  val cloud      = file"/Users/chenh/Downloads/cloud.txt".lines.flatMap(_.split(" ").map(_.toLong)).toArray
  val range      = file"/Users/chenh/Downloads/range.txt".lines.flatMap(_.split(" ").map(_.toLong)).toArray

  println(maximumPeople(population, position, cloud, range))

}
