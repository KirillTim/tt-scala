package typeTheory.term

import org.parboiled2._
import scala.util.Success

object TermParser {
  def parseEq(s: String): Option[TEq] = new ParboiledParser(s).line.run() match {
    case Success(term) => Some(term)
    case _ => None
  }

  def parseT(s: String): Option[Term] = new ParboiledParser(s).simple.run() match {
    case Success(term) => Some(term)
    case _ => None
  }

  private class ParboiledParser(val input: ParserInput) extends Parser {
    type RAlg = Rule1[Term]

    def line: Rule1[TEq] = rule { equation ~ EOI }
    def simple: RAlg = rule { term ~ EOI }

    implicit private def wspStr(s: String): Rule0 = rule { zeroOrMore(' ') ~ str(s) ~ zeroOrMore(' ') }

    private def equation: Rule1[TEq] = rule { term ~ "=" ~ term ~> TEq }

    private def term: RAlg = rule {
      capture(func) ~ "(" ~ zeroOrMore(term).separatedBy(",") ~ ")" ~> Func |
        capture(variable) ~> TVar
    }

    private def func: Rule0 = rule { anyOf("abcdefgh") ~ zeroOrMore(CharPredicate.AlphaNum) ~ zeroOrMore("'") }

    private def variable: Rule0 = rule { anyOf("ijklmnopqrstuvwxyz") ~ zeroOrMore(CharPredicate.AlphaNum) ~ zeroOrMore("'") }
  }

}
