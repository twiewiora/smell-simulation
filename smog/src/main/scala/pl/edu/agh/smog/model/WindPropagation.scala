package pl.edu.agh.smog.model

import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.Grid.{CellArray, SubcellCoordinates}
import pl.edu.agh.xinuk.model.{Obstacle, Signal}

object WindPropagation {
  def calculateSmellAddends(cells: CellArray, x: Int, y: Int): Vector[Option[Signal]] = {
    @inline def destinationCellSignal(i: Int, j: Int): Option[SmellArray] = {
      cells.lift(x + i - 1).flatMap(_.lift(y + j - 1).map(_.smell))
    }

    val crossWeight: Double = 0.5
    val straightWeight: Double = 0.5

    SubcellCoordinates.map {
      case (i, j) if i == 1 || j == 1 =>
        destinationCellSignal(i, j).map(signal =>
          signal(i)(j) + signal(i + j - 1)(i + j - 1) + signal(i - j + 1)(j - i + 1)
        )
      case (i, j) if i==0 && j == 0 =>
        destinationCellSignal(i, j).map(signal => {
          var signalSum = signal(0)(0)
          if(cells(x - 1)(y) == Obstacle)
            signalSum += (signal(0)(1) * straightWeight + signal(0)(2) * crossWeight)
          if(cells(x)(y - 1) == Obstacle)
            signalSum += (signal(1)(0) * straightWeight + signal(2)(0) * crossWeight)
          signalSum
        })
      case (i, j) if i==2 && j == 0 =>
        destinationCellSignal(i, j).map(signal => {
          var signalSum = signal(2)(0)
          if(cells(x + 1)(y) == Obstacle)
            signalSum += (signal(2)(1) * straightWeight + signal(2)(2) * crossWeight)
          if(cells(x)(y - 1) == Obstacle)
            signalSum += (signal(0)(0) * straightWeight + signal(1)(0) * crossWeight)
          signalSum
        })
      case (i, j) if i==0 && j == 2 =>
        destinationCellSignal(i, j).map(signal => {
          var signalSum = signal(0)(2)
          if(cells(x - 1)(y) == Obstacle)
            signalSum += (signal(0)(0) * straightWeight + signal(0)(1) * crossWeight)
          if(cells(x)(y + 1) == Obstacle)
            signalSum += (signal(1)(2) * straightWeight + signal(2)(2) * crossWeight)
          signalSum
        })
      case (i, j) if i==2 && j == 2 =>
        destinationCellSignal(i, j).map(signal => {
          var signalSum = signal(2)(2)
          if(cells(x + 1)(y) == Obstacle)
            signalSum += (signal(2)(0) * straightWeight + signal(2)(1) * crossWeight)
          if(cells(x)(y + 1) == Obstacle)
            signalSum += (signal(0)(2) * straightWeight + signal(1)(2) * crossWeight)
          signalSum
        })

//      case (i, j) =>
//        destinationCellSignal(i, j).map(signal => {
//          var signalSum = signal(i)(j)
//          signalSum += signal(i + j - 1)(i + j - 1) + signal(i - j + 1)(j - i + 1)
////          System.out.println(x + (j - i) / 2, y + (i + j - 2) / 2, x, y, i, j)
//          if(!outOfBounds(x + (j - i) / 2, y + (i + j - 2) / 2, cells) && cells(x + (j - i) / 2)(y + (i + j - 2) / 2) == Obstacle)
//            signalSum += signal(i / 2 + j / 2)(j /2 - i / 2 + 1) + signal(j)(2 - i)
//          if(!outOfBounds(x + (i + j - 2) / 2, y + (i - j) / 2, cells) && cells(x + (i + j - 2) / 2)(y + (i - j) / 2) == Obstacle)
//            signalSum += signal(i / 2 - j / 2 + 1)(j / 2 + i / 2) + signal(2 - j)(i)
//          signalSum
//        })

//      case (i, j) =>
//              destinationCellSignal(i, j).map(signal => {
//                signal(i)(j)
//              })
    }
  }

  def outOfBounds(a: Int, b: Int, cells: CellArray): Boolean = {
    a >= cells.length || b >= cells(0).length || a < 0 || b < 0
  }

}
