package pl.edu.agh.smog.model

import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model._

final case class WindCell(smell: SmellArray) extends SmellingCell {
  override type Self = WindCell

  override def withSmell(smell: SmellArray): WindCell = copy(smell = smell)
}


object WindCell {
  def create(initialSignal: Signal): WindCell = WindCell(Array.fill(Cell.Size, Cell.Size)(initialSignal))
}