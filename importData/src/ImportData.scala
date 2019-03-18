import kantan.csv._
import kantan.csv.ops._
import scala.io._
import scala.util._
import slick.jdbc.SQLiteProfile.api._
import scala.collection.mutable._
import scala.concurrent._
import scala.concurrent.duration._
import slick.jdbc.SQLiteProfile.backend.{DatabaseDef}
import slick.lifted.{TableQuery}
import java.time.{YearMonth}
import java.lang.Math._

object ImportData {

  def join[T](future: Future[T]): T = Await.result(future, Duration.Inf)

  def distance(lat1: Float, long1: Float, lat2: Float, long2: Float): Float = {
    val dlat  = abs(lat1  - lat2 )
    val dlong = abs(long1 - long2)
    val a = pow(sin(toRadians(dlat)/2), 2) + cos(toRadians(lat1)) * cos(toRadians(lat2)) * pow(sin(toRadians(dlong)/2), 2)
    val c = 2 * atan2(sqrt(a), sqrt(1 - a))

    6371 * c.toFloat
  }

  def main(args: Array[String]): Unit = {
    val airportsFile = args(0)
    val monthFiles   = args.drop(1)
    val db           = Database.forURL("jdbc:sqlite:network.db", driver="org.sqlite.JDBC")

    try {

      db.run(DBIO.seq(Network.schema.create))

      type airportsCSV = (String, String, String, String, String, Float, Float, String, String, String, Double)
      type monthsCSV   = (Int, Int, String, String, String, String, String, String, String, Int, Int)

      var airportsCSV = Source.fromFile(airportsFile).mkString.asCsvReader[airportsCSV]('\t', false)
      val locations = HashMap[String, (Float, Float, Int)]()
      val cluster = HashMap[String, String]()
      val regions = Set[(Option[Int], String, String, String, String)]()

      airportsCSV.foreach(_ match {
        case Left(e) => println(e); ()
        case Right((airportCode, name, city, regionAbbreviation, region, latitude, longitude, _ccode, country, geographicArea, population)) => {
          locations(airportCode) = (latitude, longitude, population.toInt)
          regions += ((None, regionAbbreviation, region, country, geographicArea))
        }
      })

      val regionIds = join(db.run((Network.regions returning Network.regions.map(_.id) ++= regions.toList).transactionally))

      val airportIds = (regions.toList, regionIds).zipped.toMap

      val locs = locations.toList.sortBy(_._2._3).reverse

      for ((airport1, (lat1, long1, pop1)) <- locs;
           (airport2, (lat2, long2, pop2)) <- locs) {
        if (distance(lat1, long1, lat2, long2) < 50 && !(cluster contains airport1) && (!(cluster contains airport2) || cluster(airport2) == airport2))
          cluster(airport1) = airport2
      }

      airportsCSV = Source.fromFile(airportsFile).mkString.asCsvReader[airportsCSV]('\t', false)

      val airports = airportsCSV.toSeq.flatMap(_ match {
        case Left(e) => println(e); None
        case Right((airportCode, name, city, regionAbbreviation, region, latitude, longitude, _ccode, country, geographicArea, population)) => {
          val regionKey = (None, regionAbbreviation, region, country, geographicArea)
          Some((airportCode, name, city, latitude, longitude, population.toInt, airportIds(regionKey), cluster(airportCode)))
        }
      })

      join(db.run((Network.airports ++= airports.to).transactionally))

      for (month <- monthFiles) {
        val monthsCSV = Source.fromFile(month).mkString.asCsvReader[monthsCSV]('\t', false)

        print(month + "\n")

        val monthData = monthsCSV.toSeq.flatMap(_ match {
          case Left(e) => println(e); None
          case Right((year, month, n1, n2, n3, n4, n5, n6, n7, _pax, monthlyFlow)) => {
            val nodes        = IndexedSeq(n1, n2, n3, n4, n5, n6, n7).filter(_ != "-")
            val origins      = nodes.init
            val destinations = nodes.tail
            val edges        = (origins, destinations).zipped.toList

            val flow = monthlyFlow.toFloat / YearMonth.of(year, month).lengthOfMonth().toFloat

            Some((flow, month, edges))
          }
        })

        val paths = monthData.map(x => (None, x._1, x._2, x._3.head._1, x._3.last._2)).toList
        print(paths.length + "\n")

        val pathIds =
          join(db.run(
            (Network.paths returning Network.paths.map(_.id) ++= paths)
              .transactionally
            )
          )

        val edges =
          for {
            (id, edges) <- (pathIds, monthData.map(_._3)).zipped
            ((origin, destination), segment) <- edges.zipWithIndex
          } yield (id, segment, origin, destination)

        join(db.run(
          (Network.edges ++= edges.toList)
            .transactionally
        ))

      }

    } finally db.close
  }
}
