package pl.edu.agh.smog.algorithm

import com.avsystem.commons
import com.avsystem.commons.SharedExtensions._
import com.avsystem.commons.misc.Opt
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

  private val obstacles = Array.ofDim[Boolean](config.gridSize, config.gridSize)

  private var chimneyLocation = (0, 0)

  override def initialGrid: (Grid, SmogMetrics) = {
    grid = Grid.empty(bufferZone)

    // Obstacles
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

    // Wind
    for (y <- 1 until config.gridSize - 1){
      grid.cells(y)(1) = WindAccessible.unapply(EmptyCell.Instance).withWind()
    }

    // Chimney
    do {
      chimneyLocation = (random.nextInt(config.gridSize - 1), random.nextInt(config.gridSize - 1))
    } while (obstacles(chimneyLocation._1)(chimneyLocation._2))

    val metrics = SmogMetrics.empty()
    (grid, metrics)
  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, SmogMetrics) = {
    val newGrid = Grid.empty(bufferZone)

    def makeMove(x: Int, y: Int): Unit = {
      if (iteration % config.chimneyFrequency == 0) {
        pollute(chimneyLocation._1, chimneyLocation._2)
      }
      grid.cells(x)(y) match {
        case Obstacle =>
          newGrid.cells(x)(y) = Obstacle
        case cell@(EmptyCell(_) | BufferCell(_)) =>
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.cells(x)(y) = cell
          }
        case WindCell(_) =>
            newGrid.cells(x)(y) = WindAccessible.unapply(EmptyCell.Instance).withWind()
        case cell: SmogCell =>
          moveSmog(cell, x, y)
      }
    }

    def isEmptyIn(grid: Grid)(i: Int, j: Int): Boolean = {
      grid.cells(i)(j) match {
        case EmptyCell(_) | BufferCell(EmptyCell(_)) => true
        case _ => false
      }
    }

    def moveSmog(cell: SmogCell, x: Int, y: Int): Unit = {
      val destinations = calculatePossibleDestinations(cell, x, y, grid)
      val destination = selectDestinationCell(destinations, newGrid)
      destination match {
        case Opt((i, j, SmogAccesible(destination))) =>
          newGrid.cells(i)(j) = destination.withSmog(cell.intensity)
          val vacated = EmptyCell(cell.smell)
          newGrid.cells(x)(y) = vacated
        case Opt((i, j, inaccessibleDestination)) =>
          throw new RuntimeException(s"Smog selected inaccessible destination ($i,$j): $inaccessibleDestination")
        case Opt.Empty =>
          newGrid.cells(x)(y) = cell.copy(cell.intensity)
      }
    }

    def calculatePossibleDestinations(cell: SmogCell, x: Int, y: Int, grid: Grid): Iterator[(Int, Int, GridPart)] = {
      val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)
      Grid.SubcellCoordinates
        .map { case (i, j) => cell.smell(i)(j) }
        .zipWithIndex
        .sorted(implicitly[Ordering[(Signal, Int)]])
        .iterator
        .map { case (_, idx) =>
          val (i, j) = neighbourCellCoordinates(idx)
          (i, j, grid.cells(i)(j))
        }
    }

    def selectDestinationCell(possibleDestinations: Iterator[(Int, Int, GridPart)], newGrid: Grid): commons.Opt[(Int, Int, GridPart)] = {
      possibleDestinations
        .map { case (i, j, current) => (i, j, current, newGrid.cells(i)(j)) }
        .collectFirstOpt {
          case (i, j, currentCell@SmogAccesible(_), SmogAccesible(_)) =>
            (i, j, currentCell)
        }
    }

    def pollute(chimneyX: Int, chimneyY: Int): Unit = {
      if (!obstacles(chimneyX)(chimneyY)) {
        val neighbour = Grid.neighbourCellCoordinates(chimneyX, chimneyY)
        for ((i, j) <- neighbour) {
          if (!obstacles(i)(j)) {
            grid.cells(i)(j) = SmogAccesible.unapply(EmptyCell.Instance).withSmog(10)
          }
        }
      }
    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } makeMove(x, y)

    (newGrid, SmogMetrics.empty())
  }
}