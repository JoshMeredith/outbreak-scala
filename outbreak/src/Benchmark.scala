import slick.jdbc.H2Profile.api._
import scala.concurrent._
import scala.concurrent.duration._
import slick.jdbc.H2Profile.backend.{DatabaseDef}

object Benchmark {

  val numNodes: Int = 2880
  val numPaths: Int = 515000

  def main(args: Array[String]): Unit = {
    val th = new ichi.bench.Thyme

    val b: () => World[SEIR.Populations] = benchSEIR()

    // th.pbench(b())
    sensitivity()
  }

  def benchSEIR(): () => World[SEIR.Populations] = {
    val nodes =
      for (i <- 0 until numNodes) yield
        SEIR.Populations(1000000, 0, 100, 0)

    val paths =
      for (m <- 1 to 12; i <- 0 until numPaths) yield
        (m, Path(i % numNodes, numNodes - (i % numNodes) - 1, IndexedSeq(), 0.3f, 1000))

    var world = World(nodes, paths)

    () => SEIR.deterministic(world, 0.1f, 0.1f, 0.1f)(181)
  }

  def sensitivity() = {
    print("sensitivity\n")

    val db = Database.forURL("jdbc:sqlite:network.db", driver="org.sqlite.JDBC")
    def join[T](future: Future[T]): T = Await.result(future, Duration.Inf)
    def query[T,Q](db: DatabaseDef, q: Query[Q, T, Seq]) = join(db.run(q.result)).toSeq

    var airports = query(db, Network.airports.map(x => (x.code, x.clusterCode, x.population))).toIndexedSeq

    print("in progress\n")

    // var edges =
    //   (for {
    //     (p, e) <- Network.paths join Network.edges
    //   } yield (p.flow, e.pathId, e.segment, e.origin, e.destination)).groupBy(x => (x._1, x._2))

    // var pathsQ =
    //   edges.map { case (ps, es) =>
    //     // val segs = es.map(_._3).max

    //     (ps._1, es.map(_._4).avg, es.map(_._5).avg)
    //   }

    // var pathsIn = query(db, pathsQ).toIndexedSeq

    var pathsIn = query(db, Network.paths.map(x => (x.flow, x.origin, x.destination, x.month))).toIndexedSeq

    print("loaded\n")

    var codes    = airports.filter(x => x._1 == x._2).map(_._1).zipWithIndex.toMap
    var inPops   = airports.filter(x => x._1 == x._2).map(x => SEIR.Populations(x._3, if (x._2 == "BOS") 100 else 0, 0, 0))
    var clusters = airports.map(x => (x._1 -> x._2)).toMap

    var paths    = pathsIn.map(x => (x._4, Path(codes(clusters(x._2)), codes(clusters(x._3)), IndexedSeq(), 0, x._1)))

    print("processed\n")

    print("Clusters: " + inPops.length + "\nPaths: " + paths.length + "\n")

    // val paths2 =
    //   for (m <- 1 to 12; i <- 0 until numPaths) yield
    //     (m, Path(i % numNodes, numNodes - (i % numNodes) - 1, IndexedSeq(), 0.3f, 1000))

    // print("Paths2: " + paths2.length + "\n")

    // val th = new ichi.bench.Thyme
    // val world = World(inPops, paths.zipWithIndex.filter{ case (x, i) => i % 2 == 0 }.map(_._1) ++ paths2.zipWithIndex.filter{ case(x,i) => i % 2 == 1 }.map(_._1))
    // th.pbench(SEIR.deterministic(world, 0.1f, 0.1f, 0.1f)(181))

    var pops = for (c <- 0 to 100 map (_.toFloat / 100)) yield {
      print(c + "\n")
      var world = SEIR.deterministic(World(inPops, paths), 0.25f * c, 0.25f, 1.0f/7.0f)
      world(180).nodes.map(x => x.infected + x.recovered).sum

      // world.map(_.nodes.map(x => x.infected).sum).take(50).toList

    }

    pops.foreach(x => print(x + "\n"))
  }
}
