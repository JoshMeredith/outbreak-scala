object SEIR {
  import Populations._

  def deterministic(
    world:         World[Populations],
    exposureRate:  Float,
    infectionRate: Float,
    recoveryRate:  Float
  ): Stream[World[Populations]] = {
    Stream.iterate(world) { world: World[Populations] =>
      world.step { here =>
        val Populations(s, e, i, r) = here.data

        val newlyExposed   = exposureRate * i * s / here.data.total
        val newlyInfected  = infectionRate * e
        val newlyRecovered = recoveryRate * i

        val fn = here.departures.totalFlow / here.data.total
        val (departureS, departureE, departureI, departureR) =
          (s * fn, e * fn, i * fn, r * fn)

        val Populations(arrivalS, arrivalE, arrivalI, arrivalR) =
          sum (here.incoming.map { case (path, origin) =>
            val f = path.flow
            val n = origin.data.total
            val Populations(sO, eO, iO, rO) = origin.data
            val infected = f * iO / n
            val quarantined = infected * 0
            val notQuarantined = infected - quarantined
            Populations(f * sO / n, f * eO / n, notQuarantined, f * rO / n + quarantined)
          })

        val dS = -newlyExposed                                  - departureS + arrivalS
        val dE =  newlyExposed - newlyInfected                  - departureE + arrivalE
        val dI =                 newlyInfected - newlyRecovered - departureI + arrivalI
        val dR =                                 newlyRecovered - departureR + arrivalR

        Populations(s + dS, e + dE, i + dI, r + dR)
      }
    }
  }

  case class Populations(
    susceptible: Float,
    exposed:     Float,
    infected:    Float,
    recovered:   Float
  ) {
    lazy val total: Float = susceptible + exposed + infected + recovered

    def +(that: Populations): Populations = (this, that) match {
      case (Populations(s1, e1, i1, r1), Populations(s2, e2, i2, r2)) =>
        Populations(s1 + s2, e1 + e2, i1 + i2, r1 + r2)
    }
  }

  object Populations {
    val zero: Populations = Populations(0, 0, 0, 0)

    def sum(xs: Seq[Populations]): Populations = {
      xs.foldLeft(zero)(_+_)
    }
  }
}
