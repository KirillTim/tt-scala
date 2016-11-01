import java.io.{File, PrintWriter}

import scala.io.Source
import typeTheory.lambda._

import scala.collection.SortedSet

object Task2 {
  def main(args: Array[String]) {
    val fileName = if (args.length == 0 || args(0) == "") "data/task2.in" else args(0)
    val pw = new PrintWriter(new File(fileName + ".out"))
    LambdaParser.lambda(Source.fromFile(fileName, "UTF-8").mkString) match {
      case Some(lambda) => pw.println(SortedSet(lambda.freeVars.toList : _*).mkString("\n"))
      case _ => pw.println("parsing error")
    }
    pw.close()
  }
}
