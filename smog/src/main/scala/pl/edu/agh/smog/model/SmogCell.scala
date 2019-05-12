package pl.edu.agh.smog.model

import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, Signal, SmellingCell}

final case class SmogCell(smell: SmellArray) extends SmellingCell {
  override type Self = SmogCell

  override def withSmell(smell: SmellArray): SmogCell = copy(smell = smell)
}

object SmogCell {
  def create(initialSignal: Signal): SmogCell = SmogCell(Array.fill(Cell.Size, Cell.Size)(initialSignal))
}