object World {
  def apply[n](nodes: IndexedSeq[n], paths: IndexedSeq[Path]): World[n] = {
    val arrivals   = paths.groupBy(_.destination)
    val departures = paths.groupBy(_.origin)

    new World[n](nodes, arrivals, departures)
  }
}

case class World[n](
  nodes:      IndexedSeq[n],
  arrivals:   Map[Int, Seq[Path]],
  departures: Map[Int, Seq[Path]]
) {
  def apply(i: Int): Node[n] = Node[n](nodes(i), arrivals(i), departures(i), this)

  def step(f: Node[n] => n): World[n] = {
    val newNodes = nodes.zipWithIndex.map { case (n, i) =>
      f(Node(n, arrivals(i), departures(i), this))
    }
    this.copy(nodes = newNodes)
  }
}
