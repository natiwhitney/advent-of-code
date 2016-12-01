/**
  * Created by nwhitney on 11/30/16.
  */

object ReadCSVInput extends App {

  println("Reading csv file input")
  val bufferedSource = io.Source.fromFile("dayOneInput.csv")
  for (line <- bufferedSource.getLines) {
    // read input, parse into list of (rotate, move) tuples
    val cols = line.split(",").map(_.trim)
    val parsedInput = cols.map(parseInput)
    for (pi <- parsedInput) {
      println(s"${pi}")
    }

    // foldLeft can run a 2-argument funtion where result of
    // the function is passed as the first arg in next invocation,
    // and second argument is the current item in the collection
    // unlike reduce, foldLeft accepts initial value
    val initCoods = (0,0,0)
    val finalCoods = parsedInput.foldLeft(initCoods)(updateCood)
    println(s"${finalCoods}")

    val numBlocks = l0norm(List(finalCoods._2,finalCoods._3))
    println(s"numBlocks:$numBlocks")
  }
  bufferedSource.close

  def parseInput(x:String) : (Char, Int) = {
    return(x.charAt(0),Integer.parseInt(x.substring(1)))
  }

  def l0norm(xs: List[Int]): Int = {
    xs match {
      case x :: tail => java.lang.Math.abs(x) + l0norm(tail) // if there is an element, add it to the sum of the tail
      case Nil => 0 // if there are no elements, then the sum is 0
    }
  }

  def rotate(x: Char, coods: (Int,Int,Int)):  (Int,Int,Int) = x match {
    case 'L' => (Math.floorMod(coods._1-1,4),coods._2,coods._3)
    case 'R' => (Math.floorMod(coods._1+1,4),coods._2,coods._3)
  }

  def move(coods: (Int,Int,Int), steps: Int) = coods._1 match {
    case 0 => (coods._1,coods._2+steps,coods._3)
    case 1 => (coods._1,coods._2,coods._3+steps)
    case 2 => (coods._1,coods._2-steps,coods._3)
    case 3 => (coods._1,coods._2,coods._3-steps)
  }

  def updateCood(coods:(Int,Int,Int), input:(Char,Int)) : (Int,Int,Int) = {
    val rotatedCoods = rotate(input._1,coods)
    return move(rotatedCoods,input._2)
  }
}

