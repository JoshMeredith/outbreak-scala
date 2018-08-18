object Benchmark {

  val numNodes: Int = 3000
  val numPaths: Int = 550000

  def main(args: Array[String]): Unit = {
    val th = new ichi.bench.Thyme

    val b: () => World = benchSEIR()

    th.pbench(b())
  }

  def benchSEIR(): () => World = {
    val nodes =
      for (i <- 0 until numNodes) yield
        NodeData(Populations(10000, 0, 100, 0))

    val paths =
      for (i <- 0 until numPaths) yield
        Path(i % numNodes, numNodes - (i % numNodes) - 1, IndexedSeq(), 0.3f, 100)

    return { () => SEIR(nodes, paths, 0.1f, 0.1f, 0.1f)(181) }
  }

}
