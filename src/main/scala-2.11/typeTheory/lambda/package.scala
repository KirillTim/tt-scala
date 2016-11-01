package typeTheory

package object lambda {

  def rename(expr: Lambda) : Lambda = {

    var count = 0

    def nextVarName(): String = {
      count += 1
      s"v_$count"
    }

    def action(expr: Lambda, ctx : Map[String, String]) : Lambda = expr match {
      case App(left, right) =>
        App(action(left, ctx), action(right, ctx))
      case Abs(Var(name), body) =>
        val newName = nextVarName()
        Abs(Var(newName), action(body, ctx + (name -> newName)))
      case Var(name) =>
        Var(ctx.getOrElse(name, name))
    }

    action(expr, Map())
  }

  def prettyPrint(expr: Lambda) : String = {
    expr match  {
      case v : Var => v.name
      case app : App => s"(${prettyPrint(app.function)} ${prettyPrint(app.argument)})"
      case abs : Abs => s"(\\${abs.variable.name}.${prettyPrint(abs.body)})"
    }
  }

  def substitute(expression: Lambda, variable: Var, toSubstitute: Lambda): Lambda = {
    expression.substitute(variable, toSubstitute) match {
      case Right(e) => e
      case Left(t) => throw new NoFreeSubstitutionException(t)
    }
  }

  class NoFreeSubstitutionException(val v: Var) extends Exception
}
