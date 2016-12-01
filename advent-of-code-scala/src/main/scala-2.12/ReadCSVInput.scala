/**
  * Created by nwhitney on 11/30/16.
  */

object ReadCSVInput extends App {

  println("Reading csv file input")
  val bufferedSource = io.Source.fromFile("dayOneInput.csv")
  for (line <- bufferedSource.getLines) {
    val cols = line.split(",").map(_.trim)
    val initCoods = (0,0,0)
    // the binary function accepts a current state, cood,
    // and the next movement instruction, and uses instruction
    // to update state of coordinate
    val updateCoods  = (col: String, cood: (Int,Int,Int)) => {
      val result = updateCood(initCoods, parseInput(col))
      result
    }
    val finalCoods = cols.foldLeft(initCoods)(_ updateCoods _)
    // this is HORRIBLE and HACKEY. what's a better
    // way to convert a tuple of ints to a List[Int]
    // map something over the tuple?
    val numBlocks = l0norm(List(finalCoods._1,finalCoods._2,finalCoods._3))
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

