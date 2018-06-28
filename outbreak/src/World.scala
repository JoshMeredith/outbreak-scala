object World {
  def apply(nodes: IndexedSeq[NodeData], paths: IndexedSeq[Path]): World = {
    val arrivals   = paths.groupBy(_.destination)
    val departures = paths.groupBy(_.origin)

    new World(nodes, arrivals, departures)
  }
}

case class World(
  nodes:      IndexedSeq[NodeData],
  arrivals:   Map[Int, Seq[Path]],
  departures: Map[Int, Seq[Path]]
) {
  def apply(i: Int): Node = Node(nodes(i), arrivals(i), departures(i), this)

  def map(f: Node => Populations): World = {
    val newNodes = nodes.par.zipWithIndex.map { case (n, i) =>
      val pops = f(Node(n, arrivals(i), departures(i), this))
      n.copy(populations = pops)
    }
    this.copy(nodes = newNodes.to)
  }
}
