object SEIR {

  object Pops{
    def unapply(pop: SEIR): Option[(Float, Float, Float, Float)] = {
      Some((pop.susceptible, pop.exposed, pop.infected, pop.recovered))
    }
  }

  def deterministic(
    world: World[SEIR]
  ): Stream[World[SEIR]] = {
    Stream.iterate(world) { world: World[SEIR] =>
      world.step { here =>
        val Pops(s, e, i, r) = here.data

        val newlyExposed   = here.data.exposureRate * i * s / here.data.total
        val newlyInfected  = here.data.infectionRate * e
        val newlyRecovered = here.data.recoveryRate * i

        val fn = here.departures.totalFlow / here.data.total
        val (departureS, departureE, departureI, departureR) =
          (s * fn, e * fn, i * fn, r * fn)

        val (arrivalS, arrivalE, arrivalI, arrivalR) =
          here.incoming.foldLeft((0f,0f,0f,0f)) { case ((sA, eA, iA, rA), (path, origin)) =>
            val f = path.flow
            val n = origin.data.total
            val Pops(sO, eO, iO, rO) = origin.data
            val infected = f * iO / n
            val quarantined = infected * 0f
            val notQuarantined = infected - quarantined
            (sA + f * sO / n, eA + f * eO / n, iA + notQuarantined, rA + f * rO / n + quarantined)
          }

        val dS = -newlyExposed                                  - departureS + arrivalS
        val dE =  newlyExposed - newlyInfected                  - departureE + arrivalE
        val dI =                 newlyInfected - newlyRecovered - departureI + arrivalI
        val dR =                                 newlyRecovered - departureR + arrivalR

        here.data(dS, dE, dI, dR)
      }
    }
  }
}

case class SEIR(
  susceptible:   Float,
  exposed:       Float,
  infected:      Float,
  recovered:     Float,
  exposureRate:  Float,
  infectionRate: Float,
  recoveryRate:  Float
) {
  lazy val total: Float = susceptible + exposed + infected + recovered

  def apply(s: Float, e: Float, i: Float, r: Float): SEIR = {
    this.copy(susceptible + s, exposed + e, infected + i, recovered + r)
  }
}
