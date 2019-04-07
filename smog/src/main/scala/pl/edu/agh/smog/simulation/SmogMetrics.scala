package pl.edu.agh.smog.simulation

import pl.edu.agh.xinuk.simulation.Metrics

final case class SmogMetrics(foraminiferaCount: Long,
                               algaeCount: Long,
                               foraminiferaDeaths: Long,
                               foraminiferaTotalEnergy: Double,
                               foraminiferaReproductionsCount: Long,
                               consumedAlgaeCount: Long,
                               foraminiferaTotalLifespan: Long,
                               algaeTotalLifespan: Long) extends Metrics {

  override def log: String = {
    s"$foraminiferaCount;$algaeCount;$foraminiferaDeaths;$foraminiferaTotalEnergy;$foraminiferaReproductionsCount;$consumedAlgaeCount;$foraminiferaTotalLifespan;$algaeTotalLifespan"
  }

  override def series: Vector[(String, Double)] = Vector(
    "Foraminifera" -> foraminiferaCount,
    "Algae" -> algaeCount
  )

  override def +(other: Metrics): SmogMetrics = {
    other match {
      case SmogMetrics.EMPTY => this
      case SmogMetrics(otherForaminiferaCount, otherAlgaeCount, otherForaminiferaDeaths, otherForaminiferaTotalEnergy,
      otherForaminiferaReproductionsCount, otherConsumedAlgaeCount, otherForaminiferaTotalLifespan,
      otherAlgaeTotalLifespan) =>
        SmogMetrics(foraminiferaCount + otherForaminiferaCount, algaeCount + otherAlgaeCount,
          foraminiferaDeaths + otherForaminiferaDeaths, foraminiferaTotalEnergy + otherForaminiferaTotalEnergy,
          foraminiferaReproductionsCount + otherForaminiferaReproductionsCount,
          consumedAlgaeCount + otherConsumedAlgaeCount, foraminiferaTotalLifespan + otherForaminiferaTotalLifespan,
          algaeTotalLifespan + otherAlgaeTotalLifespan)
      case null => this
      case _ => throw new UnsupportedOperationException(s"Cannot add: non-SmogMetrics to SmogMetrics")
    }
  }
}

object SmogMetrics {
  private val EMPTY = SmogMetrics(0, 0, 0, 0, 0, 0, 0, 0)

  def empty(): SmogMetrics = EMPTY
}