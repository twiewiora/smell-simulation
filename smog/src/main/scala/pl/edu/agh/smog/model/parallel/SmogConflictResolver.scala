package pl.edu.agh.smog.model.parallel

import pl.edu.agh.smog.config.SmogConfig
import pl.edu.agh.smog.model.{SmogCell, WindCell}
import pl.edu.agh.smog.simulation.SmogMetrics
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.parallel.ConflictResolver

object SmogConflictResolver extends ConflictResolver[SmogConfig] {

  import Cell._

  override def resolveConflict(current: GridPart, incoming: SmellingCell)(implicit config: SmogConfig): (GridPart, SmogMetrics) = {
    (current, incoming) match {
      case (Obstacle, _) =>
        (Obstacle, SmogMetrics.empty())
      case (EmptyCell(currentSmell), incomingCell) =>
        (incomingCell.withSmell(incomingCell.smell + currentSmell), SmogMetrics.empty())
      case (currentCell: SmellingCell, EmptyCell(incomingSmell)) =>
        (currentCell.withSmell(currentCell.smell + incomingSmell), SmogMetrics.empty())
      case (WindCell(currentSmell), SmogCell(_, _)) =>
        (WindCell(currentSmell), SmogMetrics.empty())
      case (SmogCell(currentSmell, currentIntensity), another@SmogCell(incomingSmell, incomingIntensity)) =>
        (SmogCell(currentSmell + incomingSmell, currentIntensity + incomingIntensity), SmogMetrics.empty())
      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}
