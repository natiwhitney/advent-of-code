/**
  * Created by natashawhitney on 12/2/16.
  */
import scala.collection.mutable.ListBuffer

object dayTwo extends App {
  // TODO: what is the idiomatic way to process file inputs
  def parseInputs(fileName:String): List[String] = {
    println("Reading csv file input")
    var parsedInputs = new ListBuffer[String]()
    val bufferedSource = io.Source.fromFile(fileName)
    for (line <- bufferedSource.getLines()) {
      parsedInputs += line
    }
    return(parsedInputs.toList)
  }

  // TODO: can you use range + partition to make grid?
  val grid = Array(Array(1,2,3), Array(4,5,6), Array(7,8,9))
  val initState = (1,1)
  var lines = parseInputs("dayTwoInput.csv").toArray
  println(s"num lines:${lines.length}")
  var movesArray = lines.map(l => l.toArray)

  def move(state: (Int,Int), dir: Char): (Int, Int) = dir match {
    case 'U' => (Math.min(Math.floorMod((state._1-1),3),state._1), state._2)
    case 'D' => (Math.max(Math.floorMod((state._1+1),3),state._1), state._2)
    case 'L' => (state._1, Math.min(Math.floorMod((state._2-1),3),state._2))
    case 'R' => (state._1, Math.max(Math.floorMod((state._2+1),3),state._2))
  }

  def updateState(state: (Int, Int), dir: Char): (Int, Int) = {
    val updatedState = move(state,dir)
    return updatedState
  }

  def processLine(button: (Int,Int), line: Array[Char]) : (Int,Int) = {
    val newButton = line.foldLeft(button)(updateState)
    return newButton
  }

  val buttons = movesArray.scanLeft(initState)(processLine)
  buttons.drop(1).map(b => println(s"button:${valForState(b)}"))

  def valForState(state: (Int,Int)): Int = {
    return grid(state._1)(state._2)
  }

}
