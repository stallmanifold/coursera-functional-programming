package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr


object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions mapValues { exprSignal => Signal {
        val expr = exprSignal()
        eval(expr, namedExpressions)
      }
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(v)    => v
      case name @ Ref(_) => eval(evalRef(name, references), references)
      case Plus(a, b)    => eval(a, references) + eval(b, references)
      case Minus(a, b)   => eval(a, references) - eval(b, references)
      case Times(a, b)   => eval(a, references) * eval(b, references)
      case Divide(a, b)  => eval(a, references) / eval(b, references)
    }
  }

  private def extractDependencies(ref: Ref, references: Map[String, Signal[Expr]]): Set[Ref] = {
    var visited = Set[Ref]()
    var exprs = extractExprs(ref, references)
    for (expr <- exprs) {
      expr match {
        case ref @ Ref(_) =>
          if (!visited.contains(ref)) {
            visited += ref
          }
        case _ =>
      }
      exprs ++= extractExprs(expr, references)
    }

    visited
  }

  private def extractRefs(expr: Expr): Set[Ref] = {
    expr match {
      case ref @ Ref(_) => Set(ref)
      case Plus(ref1 @ Ref(_), ref2 @ Ref(_)) => Set(ref1, ref2)
      case Plus(ref @ Ref(_), _) => Set(ref)
      case Plus(_, ref @ Ref(_)) => Set(ref)
      case Minus(ref1 @ Ref(_), ref2 @ Ref(_)) => Set(ref1, ref2)
      case Minus(ref @ Ref(_), _) => Set(ref)
      case Minus(_, ref @ Ref(_)) => Set(ref)
      case Times(ref1 @ Ref(_), ref2 @ Ref(_)) => Set(ref1, ref2)
      case Times(ref @ Ref(_), _) => Set(ref)
      case Times(_, ref @ Ref(_)) => Set(ref)
      case Divide(ref1 @ Ref(_), ref2 @ Ref(_)) => Set(ref1, ref2)
      case Divide(ref @ Ref(_), _) => Set(ref)
      case Divide(_, ref @ Ref(_)) => Set(ref)
      case _ => Set()
    }
  }

  private def extractExprs(expr: Expr, references: Map[String, Signal[Expr]]): Set[Expr] = {
    expr match {
      case lit @ Literal(_) => Set()
      case ref @ Ref(_)     => Set(getReferenceExpr(ref.name, references))
      case Plus(a, b)       => Set(a, b)
      case Minus(a, b)      => Set(a, b)
      case Times(a, b)      => Set(a, b)
      case Divide(a, b)     => Set(a, b)
    }
  }

  private def hasCyclicDependencies(ref: Ref, references: Map[String, Signal[Expr]]): Boolean = {
    val deps = extractDependencies(ref, references)
    deps contains ref
  }

  private def evalRef(ref: Ref, references: Map[String, Signal[Expr]]): Expr = {
    if (hasCyclicDependencies(ref, references)) Literal(Double.NaN)
    // It is safe to fetch the reference expression.
    else getReferenceExpr(ref.name, references)
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]): Expr = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
