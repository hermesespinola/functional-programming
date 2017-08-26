package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] =
    for ((variable, expr) <- namedExpressions)
      yield variable -> Signal( eval(expr(), namedExpressions) )

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def ev(expr: Expr) = eval(expr, references)
    expr match {
      case Literal(v) => v
      case Plus(a, b) => ev(a) + ev(b)
      case Minus(a, b) => ev(a) - ev(b)
      case Times(a, b) => ev(a) * ev(b)
      case Divide(a, b) => ev(a) / ev(b)
      case Ref(name) =>
        val ref = getReferenceExpr(name, references)
        eval(ref, references - name)
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}