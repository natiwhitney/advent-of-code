import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * Created by natashawhitney on 12/5/16.
  */
object dayThree extends App {
  val fileName = "dayThreeInput.csv"
  def parseInputs(fileName:String): Array[Array[Int]] = {
    println("Reading file input")
    var parsedInputs = new ArrayBuffer[Array[Int]]()
    val bufferedSource = io.Source.fromFile(fileName)
    for (line <- bufferedSource.getLines()) {
      val parsedLine = line.split(" ")
      // TODO idiomatic way to append elements to an array (without num)
      var parsedLineInputs = new ArrayBuffer[Int]
      for (input <- parsedLine) {
        if (input != " " && input != "") parsedLineInputs += Integer.parseInt(input)
      }
      // println(s"${parsedLineInputs(0)},${parsedLineInputs(1)},${parsedLineInputs(2)}")
      parsedInputs += parsedLineInputs.toArray
    }
    return parsedInputs.toArray
  }

  def parseInputsVertically(fileName:String): Array[Array[Int]] = {
    val horizontalInputs = parseInputs(fileName)

    var parsedInputs = new ArrayBuffer[Array[Int]]
    val r = (0 to (horizontalInputs.length - 1) by 3)
    val indices = Seq(0, 1, 2)
    for (rowStart <- r) {
      for (col <- 0 to 2) {
        val rowIndices = indices.map(x => rowStart + x)
        val triangleRows = rowIndices map horizontalInputs
        val triangle = triangleRows.map(x => x(col)).toArray
        println(s"triangle: ${triangle(0)},${triangle(1)},${triangle(2)}")
        parsedInputs += triangle
      }
    }
    return parsedInputs.toArray
  }

  def checkTriangleP(sides:Array[Int]): Boolean  = {
    // sort array in place
    scala.util.Sorting.quickSort(sides)
    return (sides(2) < (sides(1) + sides(0)))
  }

  def countAllTriangles(triangles: Array[Array[Int]]): Int = {
    val mapCount = triangles.map(checkTriangleP).map(B => if (B) 1 else 0)
    val count = mapCount.reduceLeft(_ + _)
    // println(s"count is: ${count}")
    return count
  }

  val partOneCount = countAllTriangles(parseInputs(fileName))
  println(s"part two count: ${partOneCount}")
  val partTwoCount = countAllTriangles(parseInputsVertically(fileName))
  println(s"part two count: ${partTwoCount}")

}
