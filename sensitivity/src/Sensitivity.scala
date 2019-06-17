
object Sensitivity {

  def main(args: Array[String]): Unit = sensitivity

  def exposureMultipler(mul: Float, pop: (SEIR.Populations, SEIR.Rates)): (SEIR.Populations, SEIR.Rates) = {
    val (ps, rs) = pop
    (ps, rs.copy(exposureRate = rs.exposureRate * mul))
  }

  def sensitivity = {
    var w = World.load.reconfigure{ case (code, pop) => (
      SEIR.Populations(if (code == "BOS") pop - 100 else pop, if (code == "BOS") 100 else 0, 0, 0),
      SEIR.Rates(0.25f, 0.25f, 1.0f/7.0f)
    )}

    var pops = for (c <- 0 to 100 map (_.toFloat / 100)) yield {
      print(c + "\n")
      var world = SEIR.deterministic(w.reconfigure(exposureMultipler(c, _)))
      world(180).nodes.map(x => x._1.infected + x._1.recovered).sum

      // world.map(_.nodes.map(x => x.infected).sum).take(50).toList

    }

    pops.foreach(x => print(x + "\n"))
  }
}
