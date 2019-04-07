package pl.edu.agh.smog

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.smog.algorithm.SmogMovesController
import pl.edu.agh.smog.config.SmogConfig
import pl.edu.agh.smog.model.parallel.SmogConflictResolver
import pl.edu.agh.smog.model.{AlgaeCell, ForaminiferaCell}
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.{DefaultSmellPropagation, SmellingCell}

object SmogMain extends LazyLogging {
  private val configPrefix = "smog"
  private val metricHeaders = Vector(
    "foraminiferaCount",
    "algaeCount",
    "foraminiferaDeaths",
    "foraminiferaTotalEnergy",
    "foraminiferaReproductionsCount",
    "consumedAlgaeCount",
    "foraminiferaTotalLifespan",
    "algaeTotalLifespan"
  )

  private def cellToColor(cell: SmellingCell): Color = {
    cell match {
      case AlgaeCell(_, _) => new Color(0, 128, 0)
      case ForaminiferaCell(_, _, _) => new Color(139, 69, 19)
      case _ => Color.WHITE
    }
  }

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation[SmogConfig](configPrefix, metricHeaders, SmogConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard)(new SmogMovesController(_)(_),
      { case cell: SmellingCell => cellToColor(cell) }
    ).start()
  }

}

