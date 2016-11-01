package typeTheory.lambda

import org.parboiled2._

import scala.util.{Failure, Success}

object LambdaParser {
  def lambda(s: String): Option[Lambda] = new MyParser(s).line.run() match {
    case Success(result) => Some(result)
    case Failure(exception) => throw exception
  }

  def substitution(s: String) : Option[(Lambda, Var, Lambda)] = new MyParser(s).substitution.run() match {
    case Success(result) => Some(result)
    case _ => None
  }

  private class MyParser(val input: ParserInput) extends Parser {
    def line: Rule1[Lambda] = rule { expr ~ EOI }

    def substitution: Rule1[(Lambda, Var, Lambda)] = rule {
      expr ~ "[" ~ variable ~ ":=" ~ expr ~ "]" ~> ((e: Lambda, v: Var, sub: Lambda) => (e, v, sub))
    }
    private def expr: Rule1[Lambda] = rule { application | atom }

    private def application: Rule1[Lambda] = rule { atom ~ oneOrMore(" " ~ atom ~> App) }

    private def atom: Rule1[Lambda] = rule { variable | abstraction | brackets }

    private def variable: Rule1[Var] = rule {
      capture(CharPredicate.LowerAlpha) ~ zeroOrMore(capture(CharPredicate.AlphaNum)) ~
        zeroOrMore(capture("'")) ~> ((s: String, seq: Seq[_], seq2: Seq[_]) => Var(s + seq.mkString + seq2.mkString))
    }

    private def brackets: Rule1[Lambda] = rule { "(" ~ expr ~ ")" }

    private def abstraction: Rule1[Lambda] = rule { "\\" ~ variable ~ wspStr(".") ~ expr ~> Abs }

    private def wspStr(s: String): Rule0 = rule { zeroOrMore(' ') ~ str(s) ~ zeroOrMore(' ') }
  }
}
