import scala.collection.mutable.{ArrayBuffer, ListBuffer}
/**
  * Created by natashawhitney on 12/5/16.
  */
object daySeven extends App {

  def isAbba(abba:String): Boolean = {
    return (abba.length <= 4) && (abba(0) != abba(1)) && (abba(0) == abba(3))  && (abba(1) == abba(2))
  }

  def isAbbaLongString(longString:String): Boolean = {
    val abbas = longString.sliding(4,1).toList.filter(isAbba)
    return abbas.length > 0
  }

  val fileName = "daySevenInput.csv"
  def parseInputs(fileName:String): List[List[String]] = {
    val bufferedSource = io.Source.fromFile(fileName)
    val parsedInputs = new ListBuffer[List[String]]()

    for (line <- bufferedSource.getLines()) {
      val parsedLine = parseLine(line)
      parsedInputs += parsedLine
    }
    return parsedInputs.toList
  }

  // multiple square brackets
  def parseLine(line:String): List[String] = {
    val lineSplits = line.split('[' )
    val lineSplits2 = lineSplits(1).split(']')
    return List(lineSplits(0),lineSplits2(0),lineSplits2(1))
  }

  def validateInput(input:List[String]): Int = {
   if ((isAbbaLongString(input(0)) || isAbbaLongString(input(2))) && !isAbbaLongString(input(1))) {
     return 1
   } else {
     return 0
   }
  }

  val parsedInputs = daySeven.parseInputs("daySevenInputs.csv")
  val validInputs = parsedInputs.map(validateInput)
  val total = validInputs.reduceLeft(_+_)
  println(s"total:$total")
}

