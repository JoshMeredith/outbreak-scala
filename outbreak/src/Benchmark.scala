
object Benchmark {

  val numNodes: Int = 2880
  val numPaths: Int = 515000

  def main(args: Array[String]): Unit = {
    val th = new ichi.bench.Thyme

    val b: () => World[SEIR.Populations] = benchSEIR

    th.pbench(b())
  }

  def benchSEIR: () => World[SEIR.Populations] = {
    val nodes =
      for (i <- 0 until numNodes) yield
        SEIR.Populations(1000000, 0, 100, 0)

    val paths =
      for (m <- 1 to 12; i <- 0 until numPaths) yield
        (m, Path(i % numNodes, numNodes - (i % numNodes) - 1, IndexedSeq(), 0.3f, 1000))

    var world = World(nodes, paths)

    () => SEIR.deterministic(world, 0.1f, 0.1f, 0.1f)(181)
  }
}
