package pl.edu.agh.smog

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.smog.algorithm.SmogMovesController
import pl.edu.agh.smog.config.SmogConfig
import pl.edu.agh.smog.model.parallel.SmogConflictResolver
import pl.edu.agh.smog.model.{SmogCell, WindPropagation}
import pl.edu.agh.xinuk.model._

object SmogMain extends LazyLogging {
  private val configPrefix = "smog"
  private val metricHeaders = Vector()

  private def cellToColor(cell: SmellingCell): Color = {
    val smellValue = cell.smell.map(_.map(_.value).max).max.toFloat
    val brightness = Math.pow(smellValue, 0.1).toFloat
    if (smellValue < 0.00001) {
      val hue = 1f
      val saturation = 1f
      Color.getHSBColor(hue, saturation, brightness)
    } else if (smellValue < 0.001) {
      val hue = 0.65f
      val saturation = 1f
      Color.getHSBColor(hue, saturation, brightness)
    } else if (smellValue < 0.1) {
      val hue = 0.28f
      val saturation = 1f
      Color.getHSBColor(hue, saturation, brightness)
    } else {
      val hue = 0.11f
      val saturation = 0.69f
      Color.getHSBColor(hue, saturation, brightness)
    }
  }

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new SmogSimulation[SmogConfig](configPrefix, metricHeaders, SmogConflictResolver,
      WindPropagation.calculateSmellAddends)((tuples, smogConfig) => new SmogMovesController(tuples)(smogConfig),
      {
        case Obstacle => Color.WHITE
        case cell: SmogCell => Color.GRAY
        case cell: SmellingCell => cellToColor(cell)
      }
    ).start()
  }

}

