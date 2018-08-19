case class Node(
  data:       NodeData,
  arrivals:   Seq[Path],
  departures: Seq[Path],
  world:      World
) {
  def populations: SEIR.Populations = data.populations

  def incoming: Seq[(Path, Node)] =
    for (p <- arrivals  ) yield (p, world(p.origin))

  def outgoing: Seq[(Path, Node)] =
    for (p <- departures) yield (p, world(p.destination))
}

case class NodeData(
  populations: SEIR.Populations
)

case class Path(
  origin:         Int,
  destination:    Int,
  nodes:          IndexedSeq[Int],
  quarantineRate: Float,
  flow:           Float
)
