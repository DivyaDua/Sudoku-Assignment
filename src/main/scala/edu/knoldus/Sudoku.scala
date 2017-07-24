package edu.knoldus

class Sudoku {

  def sudoku(grid: Vector[Vector[Int]]): Boolean = {

    def solver(grid: Vector[Vector[Int]], row:Int, col:Int, prevVal:Int = 0): Boolean = {
      if (row == 9) {println(grid.map(_.mkString(" ")).mkString("\n"))
        true}
      else if (col == 9) {solver(grid, row + 1, 0)}
      else {
        val (row,col) = emptyPlaces(grid)
        if(row == -1 && col == -1)  true
        else {
          val num = valueToFill(grid, row, col,prevVal)
          if (num == -1) false
          else {
            val updatedGrid = grid.updated(row, grid(row).updated(col, num))
            if (solver(updatedGrid, row, col + 1)) true else {solver(grid, row, col, updatedGrid(row)(col))}
          }
        }
      }
    }
    solver(grid, 0,0)

  }

  //finding values that can be assigned to empty positions
  private def valueToFill(grid: Vector[Vector[Int]], row: Int, col: Int, prevVal:Int): Int = {

    val num = List.range(prevVal+1,10)
    def assign(grid: Vector[Vector[Int]], num: List[Int], row: Int,col: Int): Int = {
      if(num.isEmpty)
        -1
      else if(checkAssignedValue(grid, row, col, num.head))
        num.head
      else
        assign(grid, num.tail, row, col)
    }
    assign(grid, num, row,col)

  }

  //searching for empty positions
  private def emptyPlaces(grid: Vector[Vector[Int]]): (Int,Int) = {

    def find(grid: Vector[Vector[Int]], row: Int, col: Int): (Int, Int) = {

      if(grid(row)(col) == 0)
        (row, col)
      else
      {
        if(row ==8 && col == 8) (-1,-1) else if(col == 8) find(grid, row+1, 0) else find(grid, row, col+1)
      }
    }
    find(grid,0,0)
  }

  //testing whether the assigned value has conflict with other filled values or not
  private def checkAssignedValue(grid: Vector[Vector[Int]], row: Int, col: Int,num:Int): Boolean = {

    if(checkRow(grid,row,num) || checkColumn(grid,col,num) || checkBox(grid,row-row%3,col-col%3,num))
      false
    else true
  }


  //testing whether the assigned value has conflict with other filled values in the row or not
  private def checkRow(grid: Vector[Vector[Int]], row:Int, num:Int): Boolean = {

    if(grid(row).contains(num)) true else false
  }

  //testing whether the assigned value has conflict with other filled values in the column or not
  private def checkColumn(grid: Vector[Vector[Int]], col:Int, num:Int): Boolean = {

    val colVector = grid.map(_(col))
    if(colVector.contains(num)) true else false
  }

  //testing whether the assigned value has conflict with other filled values in the 3x3 grid or not
  private def checkBox(grid: Vector[Vector[Int]], row:Int, col:Int, num:Int): Boolean = {

    def check(grid: Vector[Vector[Int]], row:Int, col:Int, num:Int, i:Int, j:Int): Boolean = {

      if(j==3) false
      else if (i==3) check(grid, row, col, num, 0, j+1)
      else{
        if(grid(row+i)(col+j) == num)
          true
        else
          check(grid,row,col,num,i+1,j)
      }
    }
    check(grid, row, col, num, 0, 0)
  }

}

