object Benchmark {

  val numNodes: Int = 3000
  val numPaths: Int = 550000

  def main(args: Array[String]): Unit = {
    println("Benchmark time: " + benchSEIR + "ms")
  }

  def bench[a](f: a): Long = {
    val t0 = System.nanoTime()
    print("before\n")
    val _ = f
    val t1 = System.nanoTime()
    print("after\n")

    (t1 - t0)
  }

  def benchSEIR: Long = {
    val nodes =
      for (i <- 0 until numNodes) yield
         NodeData(Populations(10000, 0, 100, 0))

    val paths =
      for (i <- 0 until numPaths) yield
        Path(i % numNodes, numNodes - (i % numNodes) - 1, IndexedSeq(), 0.3f, 100)

    bench(SEIR(nodes, paths, 0.1f, 0.1f, 0.1f, 180).toList)
  }

}
