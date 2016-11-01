import java.io.{File, PrintWriter}

import scala.io.Source
import typeTheory.lambda._
import typeTheory._
import typeTheory.Type._
import typeTheory.term.Unificator._

object Task6 {
  def main(args: Array[String]): Unit = {
    val fileName = if (args.length == 0 || args(0) == "") "data/task6.in" else args(0)
    val pw = new PrintWriter(new File(fileName + ".out"))

    val lambda = LambdaParser.lambda(Source.fromFile(fileName, "UTF-8").mkString).get

    val newLambda = rename(lambda)

    val (system, resultType) = new Inferer().buildSystem(newLambda, Map())
    unify(system) match {
      case Some(answer) =>
        val mp = answer.map(teq => termToType(teq.lhs) -> termToType(teq.rhs)).toMap
        pw.println(substituteTypes(resultType, mp))
      case _ => pw.println("Выражение не имеет типа")
    }
    pw.close()

    def substituteTypes(where: Type, what: Map[Type, Type]) : Type = where match {
      case t@AtomT(_) => what.getOrElse(t, t)
      case ArrorT(left, right) => new ArrorT(substituteTypes(left, what), substituteTypes(right, what))
    }
  }
}

