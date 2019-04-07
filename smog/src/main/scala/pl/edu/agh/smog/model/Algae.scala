package pl.edu.agh.smog.model

import pl.edu.agh.smog.config.SmogConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, GridPart, SmellingCell}

final case class AlgaeCell(smell: SmellArray, lifespan: Long) extends SmellingCell {
  override type Self = AlgaeCell

  override def withSmell(smell: SmellArray): AlgaeCell = copy(smell = smell)
}

trait AlgaeAccessible[+T <: GridPart] {
  def withAlgae(lifespan: Long): T
}

object AlgaeAccessible {

  def unapply(arg: EmptyCell)(implicit config: SmogConfig): AlgaeAccessible[AlgaeCell] =
    new AlgaeAccessible[AlgaeCell] {
      override def withAlgae(lifespan: Long): AlgaeCell = AlgaeCell(arg.smellWith(config.algaeInitialSignal), lifespan)
    }

  def unapply(arg: BufferCell)(implicit config: SmogConfig): AlgaeAccessible[BufferCell] =
    new AlgaeAccessible[BufferCell] {
      override def withAlgae(lifespan: Long): BufferCell = BufferCell(AlgaeCell(arg.smellWith(config.algaeInitialSignal), lifespan))
    }

  def unapply(arg: GridPart)(implicit config: SmogConfig): Option[AlgaeAccessible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case _ => None
  }
}
