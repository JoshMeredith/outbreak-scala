case class Node(
  data:       NodeData,
  arrivals:   Seq[Path],
  departures: Seq[Path],
  world:      World
) {
  def populations: Populations = data.populations

  def incoming: Seq[(Path, Node)] =
    for (p <- arrivals  ) yield (p, world(p.origin))

  def outgoing: Seq[(Path, Node)] =
    for (p <- departures) yield (p, world(p.destination))
}

case class NodeData(
  populations:  Populations
)

object Populations {
  val zero: Populations = Populations(0, 0, 0, 0)
}

case class Populations(
  susceptible: Float,
  exposed:     Float,
  infected:    Float,
  recovered:   Float
) {
  val total: Float = susceptible + exposed + infected + recovered

  def +(that: Populations): Populations = (this, that) match {
    case (Populations(s1, e1, i1, r1), Populations(s2, e2, i2, r2)) =>
      Populations(s1 + s2, e1 + e2, i1 + i2, r1 + r2)
  }
}

case class Path(
  origin:         Int,
  destination:    Int,
  nodes:          IndexedSeq[Int],
  quarantineRate: Float,
  flow:           Float
)
