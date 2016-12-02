/**
  * Created by nwhitney on 11/30/16.
  */

object dayOne extends App {

  val initCood = (0, 0, 0)
  val parsedInputs = parseInputs("dayOneInput.csv")
  val numBlocks = countNumBlocks(parsedInputs,initCood)

  // Initialize stuff (not sure what is right way to do in Scala)
  var noDuplicate = true
  var coodSet = scala.collection.mutable.HashSet[(Int,Int)]()
  var justCood = (0,0)
  var updatedCood = initCood
  var prevCood = initCood
  var remainingInputs = parsedInputs
  var nextInput = remainingInputs.head
  var intermediateCoods = List[(Int,Int)]()
  var duplicateCood = (0,0)

  // Horrible, hackey logic to iterate through inputs
  // until first duplicate cood is found. Then print it out.
  // Obviously this looks like a nice candidate for a recursive fn
  while (noDuplicate && ! remainingInputs.isEmpty) {
    println(s"prev cood:${updatedCood}")
    nextInput = remainingInputs.head
    println(s"parsed input:${nextInput}")
    prevCood = updatedCood
    updatedCood = updateCood(updatedCood, nextInput)
    println(s"updated cood:${updatedCood}")
    intermediateCoods = getIntermediateCoods(prevCood,updatedCood)
    while (noDuplicate && ! intermediateCoods.isEmpty) {
      duplicateCood = intermediateCoods.head
      println(s"elves passed cood:${duplicateCood}")
      // this cool method add new elem to set, and
      // returns false if elem was already in set!
      noDuplicate = coodSet add duplicateCood
      intermediateCoods = intermediateCoods.tail
    }
    remainingInputs = remainingInputs.tail
  }
  if (! noDuplicate) {
    println(s"duplicate coordinate: ${duplicateCood}")
  } else {
    println(s"elves passed no duplicate cood")
  }
  val duplicateNumBlocks = l0norm(List(duplicateCood._1,duplicateCood._2))
  println(s"num blocks:$duplicateNumBlocks")

  def justCoordinates(triple:(Int,Int,Int)) : (Int,Int) = {
    return (triple._2,triple._3 )
  }

  def getIntermediateCoods(prev:(Int,Int,Int), updated:(Int,Int,Int)): List[(Int,Int)] = {
    var prevCood = justCoordinates(prev)
    var updatedCood = justCoordinates(updated)
    if (prevCood._1 == updatedCood._1) {
      if (prevCood._2 < updatedCood._2) {
        val range = prevCood._2 to (updatedCood._2 - 1)
        return range.map( x => (prevCood._1, x)).toList
      } else {
        val range = (updatedCood._2 + 1) to prevCood._2
        return range.map( x => (prevCood._1, x)).toList
      }
    } else if (prevCood._2 == updatedCood._2) {
      if (prevCood._1 < updatedCood._1) {
        val range = prevCood._1 to (updatedCood._1 - 1)
        return range.map( y => (y, prevCood._2)).toList
      } else {
        val range = (updatedCood._1 + 1) to prevCood._1
        return range.map( y => (y, prevCood._2)).toList
      }
    } else {
      println(s"invalid input ${prevCood} and ${updatedCood} must share 1+ coordinate")
      return List[(Int,Int)]()
    }
  }

  def parseInputs(fileName:String): List[(Char, Int)] = {
    println("Reading csv file input")
    val bufferedSource = io.Source.fromFile(fileName)
    var parsedInput = Array[(Char,Int)]()
    for (line <- bufferedSource.getLines) {
      // read input (1 line), parse into list of (rotate, move) tuples
      val cols = line.split(",").map(_.trim)
      parsedInput = cols.map(parseInput)
    }
    bufferedSource.close
    return(parsedInput.toList)
  }

    def countNumBlocks(inputs: List[(Char, Int)], init: (Int, Int, Int)): Int = {
      // foldLeft can run a 2-argument function where result of
      // the function is passed as the first arg in next invocation,
      // and second argument is the current item in the collection
      // unlike reduce, foldLeft accepts initial value

      val finalCoods = inputs.foldLeft(init)(updateCood)
      println(s"${finalCoods}")
      val numBlocks = l0norm(List(finalCoods._2, finalCoods._3))
      println(s"numBlocks:$numBlocks")
      return numBlocks
    }

    def parseInput(x: String): (Char, Int) = {
      return (x.charAt(0), Integer.parseInt(x.substring(1)))
    }

    def l0norm(xs: List[Int]): Int = {
      xs match {
        case x :: tail => java.lang.Math.abs(x) + l0norm(tail) // if there is an element, add it to the sum of the tail
        case Nil => 0 // if there are no elements, then the sum is 0
      }
    }

    def rotate(x: Char, coods: (Int, Int, Int)): (Int, Int, Int) = x match {
      case 'L' => (Math.floorMod(coods._1 - 1, 4), coods._2, coods._3)
      case 'R' => (Math.floorMod(coods._1 + 1, 4), coods._2, coods._3)
    }

    def move(coods: (Int, Int, Int), steps: Int) = coods._1 match {
      case 0 => (coods._1, coods._2 + steps, coods._3)
      case 1 => (coods._1, coods._2, coods._3 + steps)
      case 2 => (coods._1, coods._2 - steps, coods._3)
      case 3 => (coods._1, coods._2, coods._3 - steps)
    }

    def updateCood(coods: (Int, Int, Int), input: (Char, Int)): (Int, Int, Int) = {
      val rotatedCoods = rotate(input._1, coods)
      return move(rotatedCoods, input._2)
    }
}


