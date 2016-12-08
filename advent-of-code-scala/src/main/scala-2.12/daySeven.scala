import scala.collection.mutable.{ArrayBuffer, ListBuffer}
/**
  * Created by natashawhitney on 12/5/16.
  */
object daySeven extends App {

  def isAbba(abba:String): Boolean = {
    return (abba(0) == abba(3)) && (abba(0) != abba(1)) && (abba(1) == abba(2))
  }

  

}

