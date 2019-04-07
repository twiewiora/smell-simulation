package pl.edu.agh.smog.model.parallel

import pl.edu.agh.smog.config.SmogConfig
import pl.edu.agh.smog.model._
import pl.edu.agh.smog.simulation.SmogMetrics
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.parallel.ConflictResolver

object SmogConflictResolver extends ConflictResolver[SmogConfig] {

  import Cell._

  override def resolveConflict(current: GridPart, incoming: SmellingCell)(implicit config: SmogConfig): (GridPart, SmogMetrics) = {
    (current, incoming) match {
      case (EmptyCell(currentSmell), incomingCell) =>
        (incomingCell.withSmell(incomingCell.smell + currentSmell), SmogMetrics.empty())
      case (currentCell: SmellingCell, EmptyCell(incomingSmell)) =>
        (currentCell.withSmell(currentCell.smell + incomingSmell), SmogMetrics.empty())
      case (AlgaeCell(currentSmell, currentLifespan), ForaminiferaCell(energy, incomingSmell, incomingLifespan)) =>
        (ForaminiferaCell(energy + config.algaeEnergeticCapacity, incomingSmell + currentSmell, incomingLifespan), SmogMetrics(0, 0, 0, 0, 0, 1, 0, currentLifespan))
      case (ForaminiferaCell(energy, currentSmell, currentLifespan), AlgaeCell(incomingSmell, incomingLifespan)) =>
        (ForaminiferaCell(energy + config.algaeEnergeticCapacity, incomingSmell + currentSmell, currentLifespan), SmogMetrics(0, 0, 0, 0, 0, 1, 0, incomingLifespan))
      case (AlgaeCell(currentSmell, lifespan), AlgaeCell(incomingSmell, incomingLifespan)) =>
        (AlgaeCell(currentSmell + incomingSmell, math.max(lifespan, incomingLifespan)), SmogMetrics.empty())
      case (ForaminiferaCell(currentEnergy, currentSmell, lifespan), ForaminiferaCell(incomingEnergy, incomingSmell, incomingLifespan)) =>
        (ForaminiferaCell(currentEnergy + incomingEnergy, currentSmell + incomingSmell, math.max(lifespan, incomingLifespan)), SmogMetrics.empty())
      case (Obstacle, _) => (Obstacle, SmogMetrics.empty())
      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}
