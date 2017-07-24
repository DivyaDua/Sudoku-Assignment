package edu.knoldus

import org.scalatest.FunSuite

class SudokuTest extends FunSuite {

  val sudoku1 = Vector(Vector(0,0,4,8,0,0,0,1,7), Vector(6,7,0,9,0,0,0,0,0), Vector(5,0,8,0,3,0,0,0,4),
    Vector(3,0,0,7,4,0,1,0,0), Vector(0,6,9,0,0,0,7,8,0), Vector(0,0,1,0,6,9,0,0,5),
    Vector(1,0,0,0,8,0,3,0,6), Vector(0,0,0,0,0,6,0,9,1), Vector(2,4,0,0,0,1,5,0,0))

  val sudoku2 = Vector(Vector(0,0,0,0,0,0,0,0,0), Vector(0,0,0,0,0,0,0,0,0), Vector(0,0,0,0,0,0,0,0,0),
    Vector(0,0,0,0,0,0,0,0,0), Vector(0,0,0,0,0,0,0,0,0), Vector(0,0,0,0,0,0,0,0,0),
    Vector(0,0,0,0,0,0,0,0,0), Vector(0,0,0,0,0,0,0,0,0), Vector(0,0,0,0,0,0,0,0,0))

  val sudoObj = new Sudoku

  test("Testing for sudoku where solution exists")
  {
    assert(sudoObj.sudoku(sudoku1))
  }

  test("Testing for sudoku with all 0s")
  {
    assert(sudoObj.sudoku(sudoku2))
  }

}
