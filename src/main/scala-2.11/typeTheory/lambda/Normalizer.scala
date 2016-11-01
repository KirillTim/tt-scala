package typeTheory.lambda

import scala.collection.mutable

class Normalizer {

  var count = 0

  def nextFreeName() = {
    count += 1
    s"v_$count"
  }

  case class Redex(v: String, body: Lambda, arg: Lambda)

  val cache = mutable.WeakHashMap[Redex, Lambda]()
  val vars = mutable.Map[String, Var]()

  def substitute(what: Lambda, varName: String, e: Lambda, scope: Set[String] = Set()): Lambda = what match {
    case v@Var(name) => if (name == varName) e else v
    case App(x, y) => App(substitute(x, varName, e, scope), substitute(y, varName, e, scope))
    case t@Abs(Var(name), body) =>
      val bodyFree = body.freeVars.map(_.name)
      if (name == varName || !bodyFree.contains(varName)) {
        t
      } else {
        val free = nextFreeName()
        val freeVar = vars.getOrElseUpdate(free, Var(free))
        val subst = substitute(substitute(body, name, freeVar, scope + free), varName, e, scope + free)
        Abs(freeVar, subst)
      }
  }

  def headNormalForm(what: Lambda, scope: Set[String] = Set()): Lambda = what match {
    case v: Var => v
    case Abs(bind, body) => Abs(bind, headNormalForm(body, scope + bind.name))
    case App(lhs, rhs) =>
      headNormalForm(lhs, scope) match {
        case Abs(v, body) =>
          cache.getOrElseUpdate(Redex(v.name, body, rhs),
            headNormalForm(substitute(body, v.name, rhs, scope + v.name), scope + v.name))
        case o => App(o, rhs)
      }
  }

  def normalForm(what: Lambda, scope: Set[String] = Set()): Lambda = what match {
    case v: Var => v
    case Abs(v, body) => Abs(v, normalForm(body, scope + v.name))
    case App(lhs, rhs) =>
      headNormalForm(lhs) match {
        case Abs(v, body) => normalForm(substitute(body, v.name, rhs, scope + v.name), scope + v.name)
        case o => App(normalForm(o), normalForm(rhs))
      }
  }
}
