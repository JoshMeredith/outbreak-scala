
object GradientDescent {

  val units = 100000f

  def main(args: Array[String]) = {
    val w = World.load

    print("World loaded.\n")

    val largeCities = w.nodes.sortBy(_._2).reverse.take(20).map(_._1).toSet

    val uncontrolledWorld = w.reconfigure{ case (code, pop) => {
      val origin = if (code == "BOS") 100 else 0
      ( SEIR.Populations(pop - origin, origin, 0, 0)
      , SEIR.Rates(0.25f, 0.25f, 1.0f/7.0f)
      )
    }}

    val baseline = SEIR.deterministic(uncontrolledWorld)(180).nodes
                       .map(x => x._1.exposed + x._1.infected + x._1.recovered).sum

    print("Baseline calculated.\n")

    val cityDeltas = for (c <- largeCities.toList) yield {
      val world = w.reconfigure{ case (code, pop) => {
        val origin     = if (code == "BOS"            ) 100f else 0f
        val vaccinated = if ((largeCities contains code) && (code == c)) units else 0f
        ( SEIR.Populations(pop - origin - vaccinated, origin, 0f, vaccinated)
        , SEIR.Rates(0.25f, 0.25f, 1.0f/7.0f)
        )
      }}

      val infection = SEIR.deterministic(world)(180).nodes
                          .map(x => x._1.exposed + x._1.infected + x._1.recovered).sum

      print((baseline + units, infection))

      (c, (baseline - infection + units).toFloat)
    }

    // val cityDeltas = List(("LCY", 100f))

    val totalDelta = cityDeltas.map(_._2).sum

    print("Deltas calculated.\n")

    // print("Total delta: " + totalDelta + "\n")

    val vaccines = cityDeltas.toMap.map{ case (code, delta) =>
        (code, (delta/totalDelta * units).toInt)
    }.withDefaultValue(0)

    print("\n")

    val controlledWorld = w.reconfigure{ case (code, pop) => {
      val origin = if (code == "BOS") 100 else 0
      ( SEIR.Populations(Math.max(pop - origin - vaccines(code), 0), origin, 0, Math.min(pop - origin, vaccines(code)))
      , SEIR.Rates(0.25f, 0.25f, 1.0f/7.0f)
      )
    }}

    // val controlledWorld = w.reconfigure{ case (code, pop) => {
    //   val origin = if (code == "BOS") 100 else 0
    //   val vaccinated = if (code == "LCY") units else 0
    //       ( SEIR.Populations(pop - origin - vaccinated, origin, 0, vaccinated)
    //       , SEIR.Rates(0.25f, 0.25f, 1.0f/7.0f)
    //       )
    // }}

    val allocatedVaccines = controlledWorld.nodes.map(x => x._1.recovered).sum

    val controlled = SEIR.deterministic(controlledWorld)(180).nodes
                         .map(x => x._1.exposed + x._1.infected + x._1.recovered).sum

    print("Controlled calculated.\n")

    val infected = baseline + allocatedVaccines - controlled
    val extra = infected - allocatedVaccines
    val vaccineEfficiency = infected/allocatedVaccines * 100f

    print(   "Baseline:   " + baseline
         + "\nBaseline+:  " + (baseline + allocatedVaccines)
         + "\nControlled: " + controlled
         + "\nAllocated:  " + allocatedVaccines
         + "\nInfected:   " + infected
         + "\nExtra:      " + extra
         + "\nEfficiency: " + vaccineEfficiency
         + "\n"
         )
  }
}
