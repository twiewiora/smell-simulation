package pl.edu.agh.smog

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.smog.algorithm.SmogMovesController
import pl.edu.agh.smog.config.SmogConfig
import pl.edu.agh.smog.model.parallel.SmogConflictResolver
import pl.edu.agh.smog.model.{AlgaeCell, ForaminiferaCell, WindPropagation}
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.{DefaultSmellPropagation, EmptyCell, Signal, SmellingCell}
import scala.math.atan
import scala.math.Pi
import scala.math.log

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

  var minSmellValue = 0.0
  var maxSmellValue = 0.0

  private def min(a: Double, b: Double): Double ={
    if (a < b) a else b
  }
  private def max(a: Double, b: Double): Double ={
    if (a > b) a else b
  }

  private def cellToColor(cell: SmellingCell): Color = {
    val smellAlgaeValue = cell.smell.foldLeft(0.0)(_ + _.foldLeft(0.0)((acc, next) => acc + max(next.value, 0.0)).toInt)
    val smellForaminiferaValue = cell.smell.foldLeft(0.0)(_ + _.foldLeft(0.0)((acc, next) => acc + min(next.value, 0.0)).toInt)
    if(minSmellValue > smellForaminiferaValue) {
      System.out.println(minSmellValue, maxSmellValue, cell.smell(0)(0))
      minSmellValue = smellForaminiferaValue
    }
    if(maxSmellValue < smellAlgaeValue) {
      System.out.println(minSmellValue, maxSmellValue, cell.smell(0)(0))
      maxSmellValue = smellAlgaeValue
    }
    cell match {
      case AlgaeCell(_, _) => new Color(0, 128, 0)
      case ForaminiferaCell(_, _, _) => new Color(139, 69, 19)
//      case EmptyCell(_) => new Color(0, 0, 255)
      case EmptyCell(_) => new Color(((atan(log(-smellForaminiferaValue)) + Pi / 2) * 255.0 / Pi).toInt, 0, ((atan(log(smellAlgaeValue)) + Pi/ 2) * 255.0 / Pi ).toInt)
      case _ => Color.WHITE
    }
  }

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation[SmogConfig](configPrefix, metricHeaders, SmogConflictResolver,
      WindPropagation.calculateSmellAddends)(new SmogMovesController(_)(_),
      { case cell: SmellingCell => cellToColor(cell) }
    ).start()
  }

}

