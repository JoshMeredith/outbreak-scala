import scala.collection.parallel.mutable._
import java.time.{LocalDate}
import slick.jdbc.H2Profile.api._
import scala.concurrent._
import scala.concurrent.duration._
import slick.jdbc.H2Profile.backend.{DatabaseDef}

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

  def load(): World[(String, Int)] = {
    def join[T](future: Future[T]): T = Await.result(future, Duration.Inf)
    def query[T,Q](db: DatabaseDef, q: Query[Q, T, Seq]) = join(db.run(q.result)).toSeq

    val db = Database.forURL("jdbc:sqlite:network.db", driver="org.sqlite.JDBC")

    var airports = query(db, Network.airports.map(x => (x.code, x.clusterCode, x.population))).toIndexedSeq

    var pathsIn = query(db, Network.paths.map(x => (x.flow, x.origin, x.destination, x.month))).toIndexedSeq

    var codes    = airports.filter(x => x._1 == x._2).map(_._1).zipWithIndex.toMap
    var inPops   = airports.filter(x => x._1 == x._2).map(x => (x._2, x._3))
    var clusters = airports.map(x => (x._1 -> x._2)).toMap

    var paths    = pathsIn.map(x => (x._4, Path(codes(clusters(x._2)), codes(clusters(x._3)), IndexedSeq(), 0, x._1)))

    World(inPops, paths)
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

  def reconfigure[a](f: n => a): World[a] = {
    this.copy(nodes = nodes.map(f))
  }
}
