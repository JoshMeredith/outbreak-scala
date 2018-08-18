object SEIR {
  def apply(
    nodes: IndexedSeq[NodeData],
    paths: IndexedSeq[Path],
    exposureRate:  Float,
    infectionRate: Float,
    recoveryRate:  Float
  ): Stream[World] = {
    Stream.iterate(World(nodes, paths)) { world: World =>
      for (here <- world) yield {
        val Populations(s, e, i, r) = here.populations

        val newlyExposed   = exposureRate * i * s / here.populations.total
        val newlyInfected  = infectionRate * e
        val newlyRecovered = recoveryRate * i

        val Populations(departureS, departureE, departureI, departureR) =
          (for ((path, _) <- here.outgoing) yield {
            val f = path.flow
            val n = here.populations.total
            Populations(f * s / n, f * e / n, f * i / n, f * r / n)
          }) . foldLeft(Populations.zero)(_+_)

        val Populations(arrivalS, arrivalE, arrivalI, arrivalR) =
          (for ((path, origin) <- here.incoming) yield {
            val f = path.flow
            val n = origin.populations.total
            val Populations(sO, eO, iO, rO) = origin.populations
            val infected = f * iO / n
            val quarantined = infected * path.quarantineRate
            val notQuarantined = infected - quarantined
            Populations(f * sO / n, f * eO / n, notQuarantined, f * rO / n + quarantined)
          }) . foldLeft(Populations.zero)(_+_)

        val deltaS = -newlyExposed                                  - departureS + arrivalS
        val deltaE =  newlyExposed - newlyInfected                  - departureE + arrivalE
        val deltaI =                 newlyInfected - newlyRecovered - departureI + arrivalI
        val deltaR =                                 newlyRecovered - departureR + arrivalR

        Populations(s + deltaS, e + deltaE, i + deltaI, r + deltaR)
      }
    }
  }
}
