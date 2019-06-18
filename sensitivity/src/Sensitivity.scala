
object Sensitivity {

  def main(args: Array[String]): Unit = sensitivity

  def exposureMultipler(mul: Float, seir: SEIR): SEIR = {
    seir.copy(exposureRate = seir.exposureRate * mul)
  }

  def sensitivity = {
    var w = World.load.reconfigure{ case (code, pop) =>
      SEIR( if (code == "BOS") pop - 100 else pop, if (code == "BOS") 100 else 0, 0, 0,
            0.25f, 0.25f, 1.0f/7.0f )
    }

    var pops = for (c <- 0 to 100 map (_.toFloat / 100)) yield {
      print(c + "\n")
      var world = SEIR.deterministic(w.reconfigure(exposureMultipler(c, _)))
      world(180).nodes.map(x => x.infected + x.recovered).sum

      // world.map(_.nodes.map(x => x.infected).sum).take(50).toList

    }

    pops.foreach(x => print(x + "\n"))
  }
}
