/**
  * Created by nwhitney on 11/30/16.
  */

object dayOne extends App {

  val initCoods = (0, 0, 0)
  val parsedInputs = parseInputs("dayOneInput.csv")
  val numBlocks = countNumBlocks(parsedInputs,initCoods)

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


