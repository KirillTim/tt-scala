import java.io.{File, PrintWriter}

import typeTheory.term.TermParser
import scala.io.Source.fromFile

import typeTheory.term.Unificator.unify

object Task5 {
  def main(args: Array[String]) {
  val fileName = if (args.length == 0 || args(0) == "") "data/task5.in" else args(0)
    val pw = new PrintWriter(new File(fileName + ".out"))
    val system = fromFile(fileName).getLines().map(TermParser.parseEq(_).get).toList

    unify(system) match {
      case Some(answer) => pw.println(answer.mkString("\n"))
      case _ => pw.println("Система неразрешима")
    }

    pw.close()
  }
}
