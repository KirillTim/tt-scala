package typeTheory.lambda

sealed trait Lambda {

  private var parent: Lambda = this

  def freeVars: Set[Var]

  def boundedVars: Set[Var]

  def substitute(v: Var, e: Lambda): Either[Var, Lambda]

}

case class Var(name: String) extends Lambda with Ordered[Var] {
  override def hashCode(): Int = name.hashCode

  override def equals(other: Any) = other match {
    case other: Var => other.name == name
    case _ => false
  }

  override def toString = name

  override def freeVars: Set[Var] = Set(this)

  override def boundedVars: Set[Var] = Set()

  override def substitute(v: Var, e: Lambda): Either[Var, Lambda] = Right(if (this == v) e else this)

  override def compare(that: Var): Int = name.compareTo(that.name)
}

case class App(function: Lambda, argument: Lambda) extends Lambda {


  override def hashCode(): Int = function.hashCode() * 17 + argument.hashCode()

  override def equals(other: Any) = other match {
    case other: App => other.function == function && other.argument == argument
    case _ => false
  }

  override def toString = prettyPrint(this)

  override def freeVars: Set[Var] = function.freeVars ++ argument.freeVars

  override def boundedVars: Set[Var] = function.boundedVars ++ argument.boundedVars

  override def substitute(v: Var, e: Lambda): Either[Var, Lambda] = (function.substitute(v, e), argument.substitute(v, e)) match {
    case (Right(l), Right(r)) => Right(App(l, r))
    case (Left(t), _) => Left(t)
    case (_, Left(t)) => Left(t)
  }
}

case class Abs(variable: Var, body: Lambda) extends Lambda {

  override def hashCode(): Int = variable.hashCode() * 37 + body.hashCode()

  override def equals(other: Any) = other match {
    case other: Abs => other.variable == variable && other.body == body
    case _ => false
  }

  override def toString = prettyPrint(this)

  override def freeVars: Set[Var] = body.freeVars - variable

  override def boundedVars: Set[Var] = body.boundedVars + variable

  override def substitute(v: Var, e: Lambda): Either[Var, Lambda] = {
    if (variable == v || e.freeVars.contains(variable))
      Left(variable)
    else
      body.substitute(v, e) match {
        case Right(in) => Right(Abs(variable, in))
        case Left(t) => Left(t)
      }
  }
}
