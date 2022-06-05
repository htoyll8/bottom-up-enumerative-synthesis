package org.bottomup.arithmetic

import Arithmetic.Value.{add, append, arrJoin, concat, endsWith, equal, index, isAlpha, isDigit, length, lower, multiply, replaceStr, split, startsWith, strToList, subtract, upper}

import org.bottomup.arithmetic.Arithmetic.Verifier.indices

import scala.collection.mutable.ListBuffer

// Source: https://www.inf.ed.ac.uk/teaching/courses/epl/2017/assignment2/Assn2.scala
// Source: https://www.inf.ed.ac.uk/teaching/courses/epl/2017/assignment2/Assn2Solution.scala

object Arithmetic {
  type Variable = String
  type Env[A] = Map[Variable, A]

  sealed trait Expr

  case class Num(n: Integer) extends Expr
  case class Plus(children: List[Expr]) extends Expr
  case class Minus(children: List[Expr]) extends Expr
  case class Times(children: List[Expr]) extends Expr

  // Booleans
  case class Bool(n: Boolean) extends Expr
  case class Eq(children: List[Expr]) extends Expr
  case class IsAlpha(children: List[Expr]) extends Expr
  case class IsDigit(children: List[Expr]) extends Expr
  case class IfThenElse(children: List[Expr]) extends Expr
  case class StartsWith(children: List[Expr]) extends Expr
  case class EndsWith(children: List[Expr]) extends Expr

  // Strings
  case class Str(s: String) extends Expr
  case class Length(children: List[Expr]) extends Expr
  case class Index(children: List[Expr]) extends Expr
  case class Concat(children: List[Expr]) extends Expr
  case class Lower(children: List[Expr]) extends Expr
  case class Upper(children: List[Expr]) extends Expr
  case class StrSplit(children: List[Expr]) extends Expr
  case class StrReplace(children: List[Expr]) extends Expr
  case class StrToList(children: List[Expr]) extends Expr

  // Variables and let-binding
  case class Var(x: Variable) extends Expr
  case class Let(x: Variable, e1: Expr, e2: Expr) extends Expr

  // Lists
  case class StringArr(children: List[Expr]) extends Expr
  case class NumArr(children: List[Expr]) extends Expr
  case class ArrAppend(children: List[Expr]) extends Expr
  case class ArrJoin(children: List[Expr]) extends Expr

  // Functions
  //  case class Lambda(children: List[Any]) extends Expr

  // Values
  abstract class Value
  case class NumV(n: Integer) extends Value
  case class BoolV(n: Boolean) extends Value
  case class CharV(n: Integer) extends Value
  case class StringV(s: String) extends Value
  case class NumArrV(c: ListBuffer[Value]) extends Value
  case class CharArrV(c: ListBuffer[Char]) extends Value
  case class StringArrV(c: ListBuffer[String]) extends Value
  case class PairV(v1: Value, v2: Value) extends Value
  case class ClosureV(env: List[Env[Value]], x: Variable, e: Expr) extends Value
  case class RecV(env: List[Env[Value]], f: Variable, x: Variable, e: Expr) extends Value

  // Types
  abstract class Type

  case object IntTy extends Type

  case object BoolTy extends Type

  case object NumArrTy extends Type

  case object StringTy extends Type

  case object StringArrTy extends Type

  case object CharArrTy extends Type

  case class PairTy(ty1: Type, ty2: Type) extends Type

  case class FunTy(ty1: Type, ty2: Type) extends Type

  case object ErrTy extends Type


  // ======================================================================
  // Section 1: Primitive operations
  // ======================================================================

  object Value {
    // utility methods for operating on values
    def add(v1: Value, v2: Value): Value = (v1, v2) match {
      case (NumV(v1), NumV(v2)) => NumV(v1 + v2)
      case _ => sys.error("Arguments to addition are non-numeric.")
    }

    def subtract(v1: Value, v2: Value): Value = (v1, v2) match {
      case (NumV(v1), NumV(v2)) => NumV(v1 - v2)
      case _ => sys.error("Arguments to subtract are non-numeric.")
    }

    def multiply(v1: Value, v2: Value): Value = (v1, v2) match {
      case (NumV(v1), NumV(v2)) => NumV(v1 * v2)
      case _ => sys.error("Arguments to multiply are non-numeric.")
    }

    def equal(v1: Value, v2: Value): Value = (v1, v2) match {
      case (BoolV(b1), BoolV(b2)) => BoolV(b1 == b2)
      case (NumV(n1), NumV(n2)) => BoolV(n1 == n2)
      case (StringV(s1), StringV(s2)) => BoolV(s1 == s2)
      case _ => sys.error("Only equality for base types is supported")
    }

    def length(v: Value): Value = v match {
      case StringV(v) => NumV(v.length)
      case StringArrV(v) => NumV(v.length)
      case NumArrV(v) => NumV(v.length)
      case _ => sys.error(s"Argument to Length, $v, is not a string.")
    }

    def index(v1: Value, v2: Value): Value = (v1, v2) match {
      case (StringV(v1), NumV(v2)) if v2 <= v1.length => StringV(v1.charAt(v2).toString)
      case (StringArrV(v1), NumV(v2)) if v2 <= v1.length => StringV(v1(v2))
      case (CharArrV(v1), NumV(v2)) if v2 <= v1.length => CharV(v1(v2))
      case (NumArrV(v1), NumV(v2)) if v2 <= v1.length => v1(v2)
      case _ => sys.error("Argument to index is not a string and index.")
    }

    def concat(v1: Value, v2: Value): Value = (v1, v2) match {
      case (StringV(v1), StringV(v2)) => StringV(v1.concat(v2))
      case _ => sys.error("Argument to concat are not strings.")
    }

    def split(v1: Value, v2: Value): Value = (v1, v2) match {
      case (StringV(v1), StringV(v2)) => StringArrV(ListBuffer[String]() ++= v1.split(v2))
      case _ => sys.error("Argument to concat are not strings.")
    }

    def replaceStr(v1: Value, v2: Value, v3: Value): Value = (v1, v2, v3) match {
      case (StringV(v1), StringV(v2), StringV(v3)) => StringV(v1.replace(v2, v3))
      case _ => sys.error("Argument to concat are not strings.")
    }

    def strToList(v1: Value): Value = v1 match {
      case (StringV(v1)) => CharArrV(ListBuffer[Char]() ++= v1.toList)
      case _ => sys.error("Argument to concat are not strings.")
    }

    def startsWith(v1: Value, v2: Value): Value = (v1, v2) match {
      case (StringV(v1), StringV(v2)) => BoolV(v1.startsWith(v2))
      case _ => sys.error("Argument to concat are not strings.")
    }

    def endsWith(v1: Value, v2: Value): Value = (v1, v2) match {
      case (StringV(v1), StringV(v2)) => BoolV(v1.endsWith(v2))
      case _ => sys.error("Argument to concat are not strings.")
    }

    def isAlpha(v1: Value): Value = v1 match {
      case StringV(v1) => BoolV(v1 != null && v1.forall(_.isLetter))
      case _ => sys.error("Argument to concat are not strings.")
    }

    def isDigit(v1: Value): Value = v1 match {
      case StringV(v1) => BoolV(v1 != null && v1.forall(_.isDigit))
      case _ => sys.error("Argument to concat are not strings.")
    }

    def lower(v1: Value): Value = v1 match {
      case StringV(v1) => StringV(v1.toLowerCase)
      case _ => sys.error("Argument to concat are not strings.")
    }

    def upper(v1: Value): Value = v1 match {
      case StringV(v1) => StringV(v1.toUpperCase())
      case _ => sys.error("Argument to concat are not strings.")
    }

    def append(v1: Value, v2: Value): Value = (v1, v2) match {
      case (StringArrV(v1), StringV(v2)) => StringArrV(v1 :+ v2)
      case _ => sys.error("Argument to concat are not strings.")
    }

    def arrJoin(v1: Value, v2: Value): Value = (v1, v2) match {
      case (StringV(v1), StringV(v2)) => StringV(v2.mkString(v1))
      case (StringV(v1), StringArrV(v2)) => StringV(v2.mkString(v1))
      case (StringV(v1), CharArrV(v2)) => StringV(v2.mkString(v1))
      case _ => sys.error("Argument to concat are not strings.")
    }
  }

  // ======================================================================
  // Section 1: Verifier
  // ======================================================================

  object Verifier {
    def indices(v1: Value, v2: Value): Boolean = (v1, v2) match {
      case (StringV(v1), NumV(v2)) if v2 >= 0 && v2 < v1.length => true
      case (StringArrV(v1), NumV(v2)) if v2 >= 0 && v2 < v1.length => true
      case (NumArrV(v1), NumV(v2)) if v2 >= 0 && v2 < v1.length => true
      case _ => false
    }
  }

  def verifyMultipleCtx(envs: List[Env[Value]], ctx: Env[Type], e: Expr) =  envs.forall(env => verify(env, ctx, e))

  def verify(env: Env[Value], ctx: Env[Type], e: Expr): Boolean = e match {
    case Times(List(e1,e2)) => eval(env,e1) != NumV(0) || eval(env,e2) != NumV(0)
    case Lower(List(e)) => eval(env,e) != StringV(" ") && eval(env,e) != StringV("")
    case Upper(List(e)) => eval(env,e) != StringV(" ") && eval(env,e) != StringV("")
    case Minus(List(e1,e2)) => e1 ne e2
    case StrReplace(List(e1,e2)) => e1 ne e2
    case StartsWith(List(e1,e2)) => e1 ne e2
    case EndsWith(List(e1,e2)) => e1 ne e2
    case StrSplit(List(e1:Str,e2:Str)) => (e1 ne e2) && !e1.s.contains(e2.s)
    case Concat(List(e1,e2)) =>
      (eval(env,e1) != StringV(" ") && eval(env,e1) != StringV("")) && (eval(env,e2) != StringV(" ") && eval(env,e2) != StringV(""))
    case Index(List(e1,e2)) => e1 match {
      case s: Str => s.s.trim.nonEmpty && indices(eval(env,s),eval(env,e2))
      case _ => indices(eval(env,e1),eval(env,e2))
    }
    case Length(List(e)) => e match {
      case s: Str => s.s.trim.nonEmpty
      case e: Expr => tyOf(ctx, e) == StringTy || tyOf(ctx, e) == StringArrTy
      case _ => false
    }
    case _ => true
  }

  // ======================================================================
  // Section 2: Evaluation
  // ======================================================================

  def evalMultipleCtx(envs: List[Env[Value]], e: Expr): List[Value] =  envs.map(env => eval(env, e))

  def eval(env: Env[Value], e: Expr): Value = e match {
    // Arithmetic
    case Num(n) => NumV(n)
    case Plus(List(e1: Expr, e2: Expr)) =>
      add(eval(env, e1), eval(env, e2))
    case Minus(List(e1: Expr, e2: Expr)) =>
      subtract(eval(env, e1), eval(env, e2))
    case Times(List(e1: Expr, e2: Expr)) =>
      multiply(eval(env, e1), eval(env, e2))

    // String
    case Str(s) => StringV(s)
    case Length(List(e)) =>
      length(eval(env,e))
    case Index(List(e1,e2)) =>
      index(eval(env,e1),eval(env,e2))
    case Concat(List(e1,e2)) =>
      concat(eval(env,e1),eval(env,e2))
    case StrSplit(List(e1,e2)) =>
      split(eval(env,e1),eval(env,e2))
    case StrReplace(List(e1,e2,e3)) =>
      replaceStr(eval(env,e1),eval(env,e2),eval(env,e3))
    case StrToList(List(e1)) =>
      strToList(eval(env,e1))

    // Booleans
    case Bool(b) => BoolV(b)
    case Eq(List(e1: Expr, e2: Expr)) =>
      equal(eval(env, e1), eval(env, e2))
    case IsAlpha(List(e: Expr)) =>
      isAlpha(eval(env,e))
    case IsDigit(List(e: Expr)) =>
      isDigit(eval(env,e))
    case Lower(List(e: Expr)) =>
      lower(eval(env,e))
    case Upper(List(e: Expr)) =>
      upper(eval(env,e))
    case StartsWith(List(e1: Expr, e2: Expr)) =>
      startsWith(eval(env, e1), eval(env, e2))
    case EndsWith(List(e1: Expr, e2: Expr)) =>
      endsWith(eval(env, e1), eval(env, e2))
    case IfThenElse(List(e: Expr, e1: Expr, e2: Expr)) =>
      eval(env, e) match {
        case BoolV(true) => eval(env, e1)
        case BoolV(false) => eval(env, e2)
        case _ => sys.error("conditional must evaluate to a boolean")
      }

    // List
    case NumArr(e: List[Expr]) =>
      val children = e.map(c => eval(env,c))
      NumArrV(ListBuffer[Value]() ++= children)
    case ArrAppend(List(e: Expr, e2: Expr)) =>
      append(eval(env,e), eval(env,e2))
    case ArrJoin(List(e: Expr, e2: Expr)) =>
      arrJoin(eval(env,e), eval(env,e2))

    // Variables + let
    case Var(x) =>
      env(x)
    case Let(x, e1, e2) =>
      eval(env + (x -> eval(env, e1)), e2)
  }

  def mkCodeMultipleCtx(envs: List[Env[Value]], e: Expr): String = mkCode(envs.head, e)

  def mkCode(env: Env[Value], e: Expr): String = e match {
    // Arithmetic
    case Num(n) => n.toString
    case Plus(List(e1: Expr, e2: Expr)) =>
      "(" + mkCode(env, e1)  + " + " + mkCode(env, e2) + ")"
    case Minus(List(e1: Expr, e2: Expr)) =>
      "(" + mkCode(env, e1)  + " - " + mkCode(env, e2) + ")"
    case Times(List(e1: Expr, e2: Expr)) =>
      "(" + mkCode(env, e1)  + " * " + mkCode(env, e2) + ")"

    // String
    case Str(s) => "\'" + s +  "\'"
    case Length(List(e: Expr)) => "len(" + mkCode(env,e) + ")"
    case StrSplit(List(e1,e2)) =>
      mkCode(env,e1) + ".split(" + mkCode(env,e2) + ")"
    case Index(List(e1,e2)) =>
      mkCode(env,e1) + "[" + mkCode(env,e2) + "]"
    case Concat(List(e1,e2)) =>
      "(" + mkCode(env,e1) + " + " + mkCode(env,e2) + ")"
    case StrReplace(List(e1,e2,e3)) =>
      mkCode(env,e1) + ".replace(" + mkCode(env,e2) + "," + mkCode(env,e3) + ")"
    case StrToList(List(e1)) =>
      "List(" + mkCode(env,e1) + ")"

    //  Bool
    case Bool(b) => b.toString
    case Eq(List(e1,e2)) =>
      "(" + mkCode(env,e1) + " == " + mkCode(env,e2) + ")"
    case IsAlpha(List(e: Expr)) =>
      mkCode(env,e) + ".isalpha()"
    case IsDigit(List(e: Expr)) =>
      mkCode(env,e) + ".isdigit()"
    case Lower(List(e: Expr)) =>
      mkCode(env,e) + ".lower()"
    case Upper(List(e: Expr)) =>
      mkCode(env,e) + ".upper()"
    case StartsWith(List(e1: Expr, e2: Expr)) =>
      mkCode(env, e1) + ".startswith(" + mkCode(env, e2) + ")"
    case EndsWith(List(e1: Expr, e2: Expr)) =>
      mkCode(env, e1) + ".endswith(" + mkCode(env, e2) + ")"
    case IfThenElse(List(e: Expr, e1: Expr, e2: Expr)) =>
      mkCode(env,e1) + " if " + mkCode(env,e) + " else " + mkCode(env,e2)

    // List
    case NumArr(e: List[Expr]) =>
      "[" + e.map(c => mkCode(env, c)) + "]"
    case ArrAppend(List(e1: Expr, e2: Expr)) =>
      "(" + mkCode(env, e1) + " += " + mkCode(env, e2) + ")"
    case ArrJoin(List(e1: Expr, e2: Expr)) =>
      "(" + mkCode(env, e1) + ".join(" + mkCode(env, e2) + ")"

    // Variables + let
    case Var(x) => x
  }

  // ======================================================================
  // Part 2: Typechecking
  // typing: calculate the return type of e, or throw an error
  // ======================================================================

  def tyOf(ctx: Env[Type], e: Expr): Type = e match {
    // Arithmetic
    case Num(_) => IntTy
    case Plus(List(e1: Expr, e2: Expr)) => (tyOf(ctx, e1), tyOf(ctx, e2)) match {
      case (IntTy, IntTy) => IntTy
      case _ => ErrTy
    }
    case Minus(List(e1: Expr, e2: Expr)) => (tyOf(ctx, e1), tyOf(ctx, e2)) match {
      case (IntTy, IntTy) => IntTy
      case _ => ErrTy
    }
    case Times(List(e1: Expr, e2: Expr)) => (tyOf(ctx, e1), tyOf(ctx, e2)) match {
      case (IntTy, IntTy) => IntTy
      case _ => ErrTy
    }

    // String
    case Str(_) => StringTy
    case Length(List(e1: Expr)) => tyOf(ctx,e1) match {
      case StringTy => IntTy
      case StringArrTy => IntTy
      case NumArrTy => IntTy
      case _ => ErrTy
    }
    case Index(List(e1: Expr, e2: Expr)) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
      case (StringTy,IntTy) => StringTy
      case (StringArrTy,IntTy) => StringTy
      case (NumArrTy,IntTy) => IntTy
      case (_,_) => ErrTy
    }
    case Concat(List(e1: Expr, e2: Expr)) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
      case (StringTy,StringTy) => StringTy
      case _ => ErrTy
    }
    case StrSplit(List(e1: Expr, e2: Expr)) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
      case (StringTy,StringTy) => StringArrTy
      case _ => ErrTy
    }
    case StrReplace(List(e1: Expr, e2: Expr, e3: Expr)) => (tyOf(ctx,e1),tyOf(ctx,e2),tyOf(ctx,e3)) match {
      case (StringTy,StringTy,StringTy) => StringTy
      case _ => ErrTy
    }
    case StrToList(List(e1: Expr)) => tyOf(ctx,e1) match {
      case (StringTy) => CharArrTy
      case _ => ErrTy
    }

    // Variables and let-binding
    case Var(x) => ctx(x)

    // Functions
    // case Lambda(List(x: Variable, ty: Type, e: Expr)) => FunTy(ty,tyOf(ctx + (x -> ty),e))

    // List
    case NumArr(_: List[Expr]) => NumArrTy
    case ArrAppend(List(e1: Expr, e2: Expr)) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
      case (StringArrTy,StringTy) => StringArrTy
      case _ => ErrTy
    }
    case ArrJoin(List(e1: Expr, e2: Expr)) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
      //case (StringTy,StringTy) => StringTy
      case (StringTy,StringArrTy) => StringTy
      case (StringTy,CharArrTy) => StringTy
      case _ => ErrTy
    }

    //  Booleans
    case Bool(_) => BoolTy
    case Eq(List(e1,e2)) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
      case (a, b) => if (a == b) {
        BoolTy
      } else {
        ErrTy
      }
    }
    case IsAlpha(List(e: Expr)) => tyOf(ctx,e) match {
      case (StringTy) => BoolTy
      case (_) => ErrTy
    }
    case IsDigit(List(e: Expr)) => tyOf(ctx,e) match {
      case (StringTy) => BoolTy
      case (_) => ErrTy
    }
    case Lower(List(e: Expr)) => tyOf(ctx,e) match {
      case (StringTy) => StringTy
      case (_) => ErrTy
    }
    case Upper(List(e: Expr)) => tyOf(ctx,e) match {
      case (StringTy) => StringTy
      case (_) => ErrTy
    }
    case StartsWith(List(e1: Expr, e2: Expr)) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
      case (StringTy,StringTy) => BoolTy
      case (_,_) => ErrTy
    }
    case EndsWith(List(e1: Expr, e2: Expr)) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
      case (StringTy,StringTy) => BoolTy
      case (_,_) => ErrTy
    }
    case IfThenElse(List(e: Expr, e1: Expr, e2: Expr)) =>
      (tyOf(ctx,e),tyOf(ctx,e1),tyOf(ctx,e2)) match {
        case (BoolTy,a,b) => if (a == b) {
          a
        }
        else {
          ErrTy
        }
        case (_,a,b) => ErrTy
      }
  }

  def canMake(e: String, childrenCandidatesTypes: List[Type]): Boolean = (childrenCandidatesTypes) match {
    case List(IntTy, IntTy) if e == "PLUS" => true
    case List(IntTy, IntTy) if e == "MINUS" => true
    case List(StringArrTy, IntTy) if e == "INDEX" => true
    case List(NumArrTy, IntTy) if e == "INDEX" => true
    case List(StringTy, IntTy) if e == "INDEX" => true
    case List(StringTy, StringTy) if e == "SPLIT" => true
    case List(StringTy, StringTy) if e == "CONCAT" => true
    case List(StringTy) if e == "LOWER" => true
    case List(StringArrTy) if e == "LENGTH" => true
    case List(NumArrTy) if e == "LENGTH" => true
    case List(StringTy) if e == "LENGTH" => true
    case List(StringTy) if e == "STRTOLIST" => true
    case List(StringArrTy) if e == "STRTOLIST" => true
    case List(StringTy,StringArrTy) if e == "ARRJOIN" => true
    case List(StringTy, StringTy) if e == "ARRJOIN" => true
    case List(StringTy,CharArrTy) if e == "ARRJOIN" => true
    case _ => false
  }

  def astSize(expr: Expr): Int = expr match {
    // Arithmetic
    case Num(_) => 1

    // String
    case Str(_) => 1
    case Var(_) => 1
    case Lower(List(e)) =>
      astSize(e) + 1
    case Length(List(e)) =>
      astSize(e) + 1
    case Index(List(e1,e2)) =>
      astSize(e1) + astSize(e2) + 1
    case Concat(List(e1,e2)) =>
      astSize(e1) + astSize(e2) + 1
    case StrSplit(List(e1,e2)) =>
      astSize(e1) + astSize(e2) + 1
    case StrReplace(List(e1,e2,e3)) =>
      astSize(e1) + astSize(e2) + astSize(e3) + 1
    case StrToList(List(e1)) =>
      astSize(e1) + 1
  }

  def aryOf(e: String): Int = e match {
    // Arithmetic
    case "PLUS"             => 2
    case "MINUS"            => 2
    case "TIMES"            => 2

    // String
    case "LENGTH"           => 1
    case "INDEX"            => 2
    case "CONCAT"           => 2
    case "SPLIT"            => 2
    case "LOWER"            => 1
    case "UPPER"            => 1
    case "STRREPLACE"       => 3
    case "STRTOLIST"        => 1

    // Boolean
    case "EQ"               => 2
    case "STARTSWITH"       => 2
    case "ENDSWITH"         => 2
    case "ISALPHA"          => 1
    case "ISDIGIT"          => 1

    // List
    case "ARRAPPEND"       => 2
    case "ARRJOIN"         => 2

    // Function
    case "LAMBDA"          => 3

    // Conditional
    case "IFTHENELSE"      => 3
   }

  def init(e: String, children: List[Expr]): Expr = e match {
    // Arithmetic
    case "PLUS"       => Plus(children)
    case "MINUS"      => Minus(children)
    case "TIMES"      => Times(children)

    // String
    case "LENGTH"     => Length(children)
    case "INDEX"      => Index(children)
    case "CONCAT"     => Concat(children)
    case "SPLIT"      => StrSplit(children)
    case "LOWER"      => Lower(children)
    case "UPPER"      => Upper(children)
    case "STRREPLACE" => StrReplace(children)
    case "STRTOLIST"  => StrToList(children)

    // Boolean
    case "EQ"         => Eq(children)
    case "STARTSWITH" => StartsWith(children)
    case "ENDSWITH"   => EndsWith(children)
    case "ISALPHA"    => IsAlpha(children)
    case "ISDIGIT"    => IsDigit(children)

    // List
    case "ARRAPPEND"  => ArrAppend(children)
    case "ARRJOIN"    => ArrJoin(children)

    // Function
    //case "LAMBDA"     => Lambda(children)

    // Conditional
    case "IFTHENELSE" => IfThenElse(children)
  }
}

