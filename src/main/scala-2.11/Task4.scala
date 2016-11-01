import java.io.{File, PrintWriter}

import typeTheory.lambda.{LambdaParser, Normalizer}

import scala.io.Source

object Task4 {
  def main(args: Array[String]) {
    val fileName = if (args.length == 0 || args(0) == "") "data/task4.in" else args(0)
    val pw = new PrintWriter(new File(fileName + ".out"))
    LambdaParser.lambda(Source.fromFile(fileName, "UTF-8").mkString) match {
      case Some(lambda) => pw.println(new Normalizer().normalForm(lambda))
      case _ => pw.println("parsing error")
    }
    pw.close()
  }
}
