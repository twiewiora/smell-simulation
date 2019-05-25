package pl.edu.agh.smog.model

import pl.edu.agh.smog.config.SmogConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model._

final case class SmogCell(intensity: Double, smell: SmellArray) extends SmellingCell {
  override type Self = SmogCell

  override def withSmell(smell: SmellArray): SmogCell = copy(smell = smell)
}

trait SmogAccesible[+T <: GridPart] {
  def withSmog(intensity: Double): T
}

object SmogAccesible {

  def unapply(arg: EmptyCell)(implicit config: SmogConfig): SmogAccesible[SmogCell] =
    (intensity: Double) => SmogCell(intensity, arg.smellWith(config.smogInitialSignal))

  def unapply(arg: BufferCell)(implicit config: SmogConfig): SmogAccesible[BufferCell] =
    (intensity: Double) => BufferCell(SmogCell(intensity, arg.smellWith(config.smogInitialSignal)))

  def unapply(arg: GridPart)(implicit config: SmogConfig): Option[SmogAccesible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case _ => None
  }

}