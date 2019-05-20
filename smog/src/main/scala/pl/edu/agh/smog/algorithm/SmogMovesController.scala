package pl.edu.agh.smog.algorithm

import pl.edu.agh.smog.config.SmogConfig
import pl.edu.agh.smog.model._
import pl.edu.agh.smog.simulation.SmogMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model._

import scala.collection.immutable.TreeSet
import scala.util.Random

final class SmogMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: SmogConfig) extends MovesController {

  private var grid: Grid = _

  private val random = new Random(System.nanoTime())

  override def initialGrid: (Grid, SmogMetrics) = {
    grid = Grid.empty(bufferZone)

    val obstacles = Array.ofDim[Boolean](config.gridSize, config.gridSize)

    for (((startX, startY), (lenX, lenY)) <- Array.fill(10){((config.gridSize * random.nextDouble(), config.gridSize * random.nextDouble()), (config.gridSize * random.nextDouble() / 2, config.gridSize * random.nextDouble() / 6))}){

      for {
        x <- startX.toInt until startX.toInt + lenX.toInt
        y <- startY.toInt until startY.toInt + lenY.toInt
        if x > 0 && y > 0 && x < config.gridSize - 1 && y < config.gridSize - 1
      }{
        obstacles(x)(y) = true
      }
    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x != 0 && y != 0 && x != config.gridSize - 1 && y != config.gridSize - 1
    } {
      if (obstacles(x)(y)){
        grid.cells(x)(y) = Obstacle
      }
    }
    grid.cells(1)(1) = SmogCell.create(config.smogInitialSignal)
    for (y <- 1 until config.gridSize - 1){
      grid.cells(y)(1) = WindCell.create(config.windInitialSignal)
    }
//    grid.cells(config.gridSize - 1)(config.gridSize - 1) = WindCell.create(config.windInitialSignal)

    val metrics = SmogMetrics.empty()
    (grid, metrics)
  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, SmogMetrics) = {
    val newGrid = Grid.empty(bufferZone)

    def copyCells(x: Int, y: Int, cell: GridPart): Unit = {
      newGrid.cells(x)(y) = cell match {
        case WindCell(smell) => WindCell.create(config.windInitialSignal)
        case _ => cell
      }
    }

    def moveCells(x: Int, y: Int, cell: GridPart): Unit = {
      val destination = (x - random.nextInt(2) + 1, y - random.nextInt(2) + 1)
      val vacatedCell = EmptyCell(cell.smell)
      val occupiedCell = SmogCell.create(config.smogInitialSignal)

      newGrid.cells(destination._1)(destination._2) match {
        case EmptyCell(_) =>
          newGrid.cells(x)(y) = vacatedCell
          newGrid.cells(destination._1)(destination._2) = occupiedCell
        case BufferCell(EmptyCell(_)) =>
          newGrid.cells(x)(y) = vacatedCell
          newGrid.cells(destination._1)(destination._2) = BufferCell(occupiedCell)
        case _ =>
          newGrid.cells(x)(y) = occupiedCell
      }
    }

    val (dynamicCells, staticCells) = (for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } yield (x, y, grid.cells(x)(y))).partition({
      case (_, _, SmogCell(_)) => true
      case (_, _, WindCell(_)) => false
      case (_, _, _) => false
    })

    staticCells.foreach({ case (x, y, cell) => copyCells(x, y, cell) })
    dynamicCells.foreach({ case (x, y, cell) => moveCells(x, y, cell) })
    (newGrid, SmogMetrics.empty())
  }
}