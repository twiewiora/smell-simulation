package pl.edu.agh.smog.model

import pl.edu.agh.smog.config.SmogConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model._

final case class WindCell(smell: SmellArray) extends SmellingCell {
  override type Self = WindCell

  override def withSmell(smell: SmellArray): WindCell = copy(smell = smell)
}

trait WindAccessible[+T <: GridPart] {
  def withWind(): T
}
object WindAccessible {

  def unapply(arg: EmptyCell)(implicit config: SmogConfig): WindAccessible[WindCell] =
    () => WindCell(arg.smellWith(config.windInitialSignal))

  def unapply(arg: GridPart)(implicit config: SmogConfig): Option[WindAccessible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case _ => None
  }
}