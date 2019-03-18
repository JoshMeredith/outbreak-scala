import slick.jdbc.SQLiteProfile.api._
import java.sql.Date

object Network {
  val airports   = TableQuery[Airports]
  val regions    = TableQuery[Regions]
  val states     = TableQuery[SimulationStates]
  val parameters = TableQuery[Parameters]
  val outbreaks  = TableQuery[Outbreaks]
  val paths      = TableQuery[Paths]
  val edges      = TableQuery[Edges]
  val schema     = airports.schema ++ regions.schema ++ states.schema ++
                   parameters.schema ++ outbreaks.schema ++ paths.schema ++
                   edges.schema

  class Airports(tag: Tag) extends Table[(String, String, String, Float, Float, Int, Int, String)](tag, "AIRPORTS") {
    def code = column[String]("AIRPORT_CODE", O.PrimaryKey)
    def name = column[String]("AIRPORT_NAME")
    def city = column[String]("CITY_NAME")
    def latitude = column[Float]("LATITUDE")
    def longitude = column[Float]("LONGITUDE")
    def population = column[Int]("POPULATION")
    def regionId = column[Int]("REGION_ID")
    def clusterCode = column[String]("CLUSTER_CODE")
    def cluster = foreignKey("CLUSTER_FK", clusterCode, Network.airports)(_.code)
    def region = foreignKey("REGION_FK", regionId, Network.regions)(_.id)
    def * = (code, name, city, latitude, longitude, population, regionId, clusterCode)
  }

  class Regions(tag: Tag) extends Table[(Option[Int], String, String, String, String)](tag, "REGIONS") {
    def id = column[Int]("REGION_ID", O.PrimaryKey, O.AutoInc)
    def abbreviation = column[String]("ABBREVIATION")
    def name = column[String]("NAME")
    def country = column[String]("COUNTRY")
    def geographicArea = column[String]("GEOGRAPHIC_AREA")
    def * = (id.?, abbreviation, name, country, geographicArea)
  }

  class SimulationStates(tag: Tag) extends Table[(String, Int, Int, Float, Float, Float, Float)](tag, "SIMULATION_STATES") {
    def clusterCode = column[String]("CLUSTER_CODE")
    def timeStep = column[Int]("TIME_STEP")
    def month = column[Int]("MONTH")
    def susceptible = column[Float]("SUSCEPTIBLE")
    def exposed = column[Float]("EXPOSED")
    def infected = column[Float]("INFECTED")
    def recovered = column[Float]("RECOVERED")
    def pk = primaryKey("PK_SIMULATION_STATES", (clusterCode, timeStep))
    def cluster = foreignKey("CLUSTER_FK", clusterCode, Network.airports)(_.code)
    def * = (clusterCode, timeStep, month, susceptible, exposed, infected, recovered)
  }

  class Parameters(tag: Tag) extends Table[(Option[Int], Float, Float, Date, Int)](tag, "PARAMETERS") {
    def id = column[Int]("PARAMETERS_ID", O.PrimaryKey, O.AutoInc)
    def infectionRate = column[Float]("INFECTION_RATE")
    def recoveryRate = column[Float]("RECOVERY_RATE")
    def date = column[Date]("DATE")
    def budget = column[Int]("BUDGET")
    def * = (id.?, infectionRate, recoveryRate, date, budget)
  }

  class Outbreaks(tag: Tag) extends Table[(Option[Int], String, Int, Date)](tag, "OUTBREAK") {
    def id = column[Int]("OUTBREAK_ID", O.PrimaryKey, O.AutoInc)
    def clusterCode = column[String]("CLUSTER_CODE")
    def population = column[Int]("POPULATION")
    def date = column[Date]("DATE")
    def cluster = foreignKey("CLUSTER_FK", clusterCode, Network.airports)(_.code)
    def * = (id.?, clusterCode, population, date)
  }

  class Paths(tag: Tag) extends Table[(Option[Int], Float, Int, String, String)](tag, "PATHS") {
    def id = column[Int]("PATH_ID", O.PrimaryKey, O.AutoInc)
    def flow = column[Float]("FLOW")
    def month = column[Int]("MONTH")
    def origin = column[String]("ORIGIN_AIRPORT")
    def destination = column[String]("DESTINATION_AIRPORT")
    def from = foreignKey("ORIGIN_FK", origin, Network.airports)(_.code)
    def to = foreignKey("DESTINATION_FK", destination, Network.airports)(_.code)
    def * = (id.?, flow, month, origin, destination)
  }

  class Edges(tag: Tag) extends Table[(Int, Int, String, String)](tag, "EDGES") {
    def pathId = column[Int]("PATH_ID")
    def segment = column[Int]("SEGMENT")
    def pk = primaryKey("PK_EDGES", (pathId, segment))
    def origin = column[String]("ORIGIN_AIRPORT")
    def destination = column[String]("DESTINATION_AIRPORT")
    def path = foreignKey("PATH_FK", pathId, Network.paths)(_.id)
    def from = foreignKey("ORIGIN_FK", origin, Network.airports)(_.code)
    def to = foreignKey("DESTINATION_FK", destination, Network.airports)(_.code)
    def * = (pathId, segment, origin, destination)
  }

}
