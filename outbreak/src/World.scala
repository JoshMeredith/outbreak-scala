import scala.collection.parallel.mutable._
import java.time._

object World {
  def apply[n](nodes: IndexedSeq[n], paths: IndexedSeq[(Int, Path)]): World[n] = {

    val arrivals = paths.groupBy(x => (x._1, x._2.destination)).mapValues { case ps =>
      Paths(ps.map(_._2))
    }.withDefault(x => Paths(Seq()))

    val departures = paths.groupBy(x => (x._1, x._2.origin)).mapValues { case ps =>
      Paths(ps.map(_._2))
    }.withDefault(x => Paths(Seq()))

    val arrivalsV   = (0 to 11).toVector.map( m =>
      (0 until nodes.length).toVector.map( i => arrivals  (m + 1, i) ))

    val departuresV = (0 to 11).toVector.map( m =>
      (0 until nodes.length).toVector.map( i => departures(m + 1, i) ))

    new World[n](nodes, arrivalsV, departuresV, LocalDate.of(2009, 6, 1))
  }
}

case class World[n](
  nodes:      IndexedSeq[n],
  arrivals:   Vector[Vector[Paths]],
  departures: Vector[Vector[Paths]],
  date:       LocalDate
) {
  private val m = date.getMonthValue - 1

  def apply(i: Int): Node[n] =
    Node[n](nodes(i), arrivals(m)(i), departures(m)(i), this)

  def step(f: Node[n] => n): World[n] = {
    var newNodes = nodes.zipWithIndex.par.map { case (n, i) =>
      f(Node(n, arrivals(m)(i), departures(m)(i), this))
    }
    this.copy(nodes = newNodes.toVector, date = date.plusDays(1))
  }
}
