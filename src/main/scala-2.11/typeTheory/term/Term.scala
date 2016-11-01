package typeTheory.term

sealed trait Term {
  def vars: Set[TVar] = this match {
    case v @ TVar(_) => Set(v)
    case Func(_, args) => args.flatMap(_.vars).toSet
    case TEq(l,r) => l.vars ++ r.vars
  }

  def contains(what: TVar): Boolean = this match {
    case v @ TVar(_) => v == what
    case Func(_, args) => args.exists { _.contains(what) }
    case TEq(lhs, rhs) => lhs.contains(what) || rhs.contains(what)
  }

  def subs(v: TVar, t: Term): Term = this match {
    case TVar(_) if this == v => t
    case Func(name, args) => Func(name, args.map(_.subs(v, t)))
    case _ => this
  }

  override def toString: String = this match {
    case TVar(name) => name
    case Func(name, args) => s"$name(${args.mkString(", ")})"
    case TEq(lhs, rhs) => s"$lhs = $rhs"
  }
}

case class TEq(lhs: Term, rhs: Term) extends Term {
  def swap: TEq = this match {
    case TEq(f @ Func(_, _), v @ TVar(_)) => TEq(v, f)
    case _ => this
  }

  def isSame: Boolean = this match {
    case TEq(f @ Func(_, _), g @ Func(_, _)) => f.name == g.name && f.args.length == g.args.length
    case _ => false
  }

  def isDiff: Boolean = this match {
    case TEq(f @ Func(_, _), g @ Func(_, _)) => f.name != g.name || f.args.length != g.args.length
    case _ => false
  }

  def unifyArgs: Seq[TEq] = this match {
    case TEq(f @ Func(_, _), g @ Func(_, _)) if isSame => (f.args, g.args).zipped.map { TEq }
    case _ => Seq(this)
  }

  def substitute(v: TVar, t: Term): TEq = TEq(lhs.subs(v, t), rhs.subs(v, t))

}
final case class Func(name: String, args: Seq[Term]) extends Term
final case class TVar(name: String) extends Term
