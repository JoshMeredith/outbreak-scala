
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
    var w = World.load.reconfigure{ case (code, pop) => SEIR.Populations(pop, if (code == "BOS") 100 else 0, 0, 0) }

    var pops = for (c <- 0 to 100 map (_.toFloat / 100)) yield {
      print(c + "\n")
      var world = SEIR.deterministic(w, 0.25f * c, 0.25f, 1.0f/7.0f)
      world(180).nodes.map(x => x.infected + x.recovered).sum

      // world.map(_.nodes.map(x => x.infected).sum).take(50).toList

    }

    pops.foreach(x => print(x + "\n"))
  }
}
