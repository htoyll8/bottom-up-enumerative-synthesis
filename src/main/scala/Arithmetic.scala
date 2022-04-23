package org.bottomup.arithmetic

// Source: https://www.inf.ed.ac.uk/teaching/courses/epl/2017/assignment2/Assn2.scala
// Source: https://www.inf.ed.ac.uk/teaching/courses/epl/2017/assignment2/Assn2Solution.scala

object Arithmetic {
  type Variable = String
  type Env[A] = Map[Variable,A]

  sealed trait Expr
  case class Num(n: Integer) extends Expr
  case class Plus(e1: Expr, e2: Expr) extends Expr
  case class Minus(e1: Expr, e2: Expr) extends Expr
  case class Times(e1: Expr, e2: Expr) extends Expr

  // Booleans
  case class Bool(n: Boolean) extends Expr
  case class Eq(e1: Expr, e2:Expr) extends Expr
  case class IfThenElse(e: Expr, e1: Expr, e2: Expr) extends Expr

  // Strings
  case class Str(s: String) extends Expr
  case class Length(e: Expr) extends Expr
  case class Index(e1: Expr, e2: Expr) extends Expr
  case class Concat(e1: Expr, e2: Expr) extends Expr

  // Variables and let-binding
  case class Var(x: Variable) extends Expr
  case class Let(x: Variable, e1: Expr, e2: Expr) extends Expr
  case class LetFun(f: Variable, arg: Variable, ty: Type, e1:Expr, e2:Expr)
    extends Expr
  case class LetRec(f: Variable, arg: Variable, xty: Type, ty: Type, e1:Expr, e2:Expr)
    extends Expr
  case class LetPair(x: Variable,y: Variable, e1:Expr, e2:Expr) extends Expr

  // Pairing
  case class Pair(e1: Expr, e2: Expr) extends Expr
  case class First(e: Expr) extends Expr
  case class Second(e: Expr) extends Expr

  // Functions
  case class Lambda(x: Variable, ty: Type, e: Expr) extends Expr
  case class Apply(e1: Expr, e2: Expr) extends Expr
  case class Rec(f: Variable, x: Variable, tyx:Type, ty: Type, e: Expr) extends Expr

  // Values
  abstract class Value
  case class NumV(n: Integer) extends Value
  case class BoolV(n: Boolean) extends Value
  case class StringV(s: String) extends Value
  case class PairV(v1: Value, v2: Value) extends Value
  case class ClosureV(env: Env[Value], x: Variable, e: Expr) extends Value
  case class RecV(env: Env[Value], f:Variable, x: Variable, e: Expr) extends Value

  // Types
  abstract class Type
  case object IntTy extends Type
  case object BoolTy extends Type
  case object StringTy extends Type
  case class PairTy(ty1: Type, ty2: Type) extends Type
  case class FunTy(ty1: Type, ty2: Type) extends Type


  // ======================================================================
  // Section 1: Primitive operations
  // ======================================================================

  object Value {
    // utility methods for operating on values
    def add(v1: Value, v2: Value): Value = (v1,v2) match {
      case (NumV(v1), NumV(v2)) => NumV(v1 + v2)
      case _ => sys.error("Arguments to addition are non-numeric.")
    }

    def subtract(v1: Value, v2: Value): Value = (v1,v2) match {
      case (NumV(v1), NumV(v2)) => NumV(v1 - v2)
      case _ => sys.error("Arguments to addition are non-numeric.")
    }

    def multiply(v1: Value, v2: Value): Value = (v1,v2) match {
      case (NumV(v1), NumV(v2)) => NumV(v1 * v2)
      case _ => sys.error("Arguments to multiply are non-numeric.")
    }

    def eq(v1: Value, v2: Value): Value =
      sys.error("Eq: todo")

    def length(v: Value): Value = v match {
      case StringV(v) => NumV(v.length)
      case _ => sys.error("Argument to Length is not a string.")
    }

    def index(v1: Value, v2: Value): Value = (v1,v2) match {
      case(StringV(v1), NumV(v2)) if v2 <= v1.length => StringV(v1.substring(v2))
      case _ => sys.error("Argument to index is not a string and index.")
    }

    def concat(v1: Value, v2: Value): Value = (v1,v2) match {
      case(StringV(v1), StringV(v2)) => StringV(v1.concat(v2))
      case _ => sys.error("Argument to concat are not strings.")
    }
  }

  // ======================================================================
  // Section 2: Evaluation
  // ======================================================================

  def eval (env: Env[Value], e: Expr): Value = e match {
    // Arithmetic
    case Num(n) => NumV(n)
    case Plus(e1,e2) =>
      Value.add(eval(env,e1),eval(env,e2))
    case Minus(e1,e2) =>
      Value.subtract(eval(env,e1),eval(env,e2))
    case Times(e1,e2) =>
      Value.multiply(eval(env,e1),eval(env,e2))

    case _ => sys.error("eval: todo")
  }
}


