package typeTheory.term
//http://www.nsl.com/misc/papers/martelli-montanari.pdf
object Unificator {
  def unify(system: Seq[TEq]): Option[Seq[TEq]] = {
    val allVars = system.flatMap(_.vars).toSet
    val res = unify_(system)
    res match {
      case Some(list) =>
        val left = list.map(_.lhs).toSet
        val more = allVars.filter(!left.contains(_)).map(p => TEq(p,p)).toSeq
        Some(more ++ list)
      case None => None
    }
  }


  private def unify_(system: Seq[TEq]): Option[Seq[TEq]] = system match {
    case Nil => Some(Seq())
    case eqs if eqs.exists { eq => eq.isSame } => unify_(eqs.flatMap(_.unifyArgs))
    case eqs if eqs.exists { eq => eq.isDiff } => None
    case eqs if eqs.exists { eq => eq.lhs == eq.rhs } => unify_(eqs.filter { eq => eq.lhs != eq.rhs })
    case eqs if eqs.exists { term => term.swap != term } => unify_(eqs.map(_.swap))
    case eqs if eqs.exists {
      case eq@TEq(v@TVar(_), Func(_, args)) => args.exists {
        _.contains(v)
      }
      case _ => false
    } => None
    case eqs =>
      val tmp = eqs.find {
        case eq@TEq(v@TVar(_), t) => v != t && eqs.filter(_ != eq).exists {
          _.contains(v)
        }
        case _ => false
      }
      tmp match {
        case Some(e@TEq(v@TVar(_), t)) => unify_(TEq(v, t) +: eqs.filter(_ != e).map(_.substitute(v, t)))
        case _ => Some(eqs)
      }
  }
}
