import java.io.{File, PrintWriter}

import typeTheory.lambda._

import scala.io.Source

object Task3 {
  def main(args: Array[String]) {
    val fileName = if (args.length == 0 || args(0) == "") "data/task3.in" else args(0)
    val pw = new PrintWriter(new File(fileName + ".out"))
    LambdaParser.substitution(Source.fromFile(fileName, "UTF-8").mkString) match {
      case Some((expr, v, sub)) =>
        try {
          pw.println(prettyPrint(substitute(expr, v, sub)))
        } catch {
          case e : NoFreeSubstitutionException => pw.println("Нет свободы для подстановки для переменной " + e.v)
        }
      case _ => pw.println("parsing error")
    }
    pw.close()
  }
}
