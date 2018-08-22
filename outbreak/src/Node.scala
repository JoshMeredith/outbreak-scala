case class Node[n](
  data:       n,
  arrivals:   Paths,
  departures: Paths,
  world:      World[n]
) {
  def incoming: Seq[(Path, Node[n])] =
    for (p <- arrivals  ) yield (p, world(p.origin))

  def outgoing: Seq[(Path, Node[n])] =
    for (p <- departures) yield (p, world(p.destination))
}

case class Path(
  origin:         Int,
  destination:    Int,
  nodes:          IndexedSeq[Int],
  quarantineRate: Float,
  flow:           Float
)

case class Paths(
  paths: Seq[Path]
) {
  def map[a](f: Path => a): Seq[a] = paths.map(f)

  lazy val totalFlow: Float =
    paths.map{_.flow}.sum
}
