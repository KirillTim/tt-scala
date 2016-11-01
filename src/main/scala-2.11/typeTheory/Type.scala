package typeTheory

import typeTheory.lambda._
import typeTheory.term.{Func, TEq, TVar, Term}

sealed trait Type

case class AtomT(name: String) extends Type {
  override def toString = name
}

case class ArrorT(from: Type, to: Type) extends Type {
  override def toString: String = (from, to) match {
    case (a@ArrorT(_, _), b) => s"($a) -> $b"
    case (a@AtomT(_), b) => s"$a -> $b"
  }
}

object Type {
  def typeToTerm(t: Type): Term = t match {
    case AtomT(name) => TVar(name)
    case ArrorT(from, to) => Func("->", Seq(typeToTerm(from), typeToTerm(to)))
  }

  def termToType(t: Term): Type = t match {
    case TVar(name) => AtomT(name)
    case Func("->", Seq(from, to)) => ArrorT(termToType(from), termToType(to))
  }
}

class Inferer() {
  import Type._
  var typeCount = 0

  def nextType(): AtomT = {
    typeCount += 1
    AtomT(s"a$typeCount")
  }

  def buildSystem(expr: Lambda, ctx: Map[Var, Type]): (Seq[TEq], Type) = expr match {
    case v@Var(_) => (Seq(), ctx.getOrElse(v, nextType()))
    case App(fun, arg) =>
      val (sysF, typeF) = buildSystem(fun, ctx)
      val (sysA, typeA) = buildSystem(arg, ctx)
      val retType = nextType()
      (sysF ++ sysA :+ TEq(typeToTerm(typeF), typeToTerm(ArrorT(typeA, retType))), retType)
    case Abs(v, body) =>
      val (_, typeV) = buildSystem(v, ctx)
      val (sysB, typeB) = buildSystem(body, ctx + (v -> typeV))
      (sysB, ArrorT(typeV, typeB))
  }
}