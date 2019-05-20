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
      case (EmptyCell(currentSmell), EmptyCell(incomingSmell)) =>
        (EmptyCell(currentSmell + incomingSmell), SmogMetrics.empty())
      case (SmogCell(currentSmell), EmptyCell(incomingSmell)) =>
        (SmogCell(currentSmell + incomingSmell), SmogMetrics.empty())
      case (EmptyCell(currentSmell), SmogCell(incomingSmell)) =>
        (SmogCell(currentSmell + incomingSmell), SmogMetrics.empty())
      case (SmogCell(currentSmell), SmogCell(incomingSmell)) =>
        (SmogCell(currentSmell + incomingSmell), SmogMetrics.empty())
      case (WindCell(currentSmell), _) =>
        (WindCell(currentSmell), SmogMetrics.empty())
      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}
