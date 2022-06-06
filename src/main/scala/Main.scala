package org.bottomup.arithmetic

import Arithmetic.{Bool, EndsWith, Env, Eq, ErrTy, Expr, IntTy, IsAlpha, IsDigit, Lower, Minus, Num, NumV, Plus, Str, StrReplace, StrSplit, StringTy, StringV, Times, Type, Value, Var, Variable, aryOf, canMake, eval, init, mkCode, tyOf, verify}

import scala.collection.mutable


object Main {
  def main(args: Array[String]): Unit = {
    val vocab = Vocab(List(
      Var("str"),
      Str(""),
      Str("<"),
      Str(">"),
    ),
      mutable.Map(
        //"CONCAT" -> 0.7,
        "STRREPLACE" -> 0.8,
      ))

    val ctx = List(
      Map(
        Var("str").x -> StringV("a < 4 and a > 0")
      ),
      Map(
        Var("str").x -> StringV("<open and <close>")
      )
    )

    val typeCtx = Map(
      Var("str").x -> StringTy,
    )

    val res = List(
      StringV("a 4 and a 0"),
      StringV("open and close")
    )

    Enumeration.enumerateLeafNodes(vocab, typeCtx, ctx)
    Enumeration.enumerate(vocab, typeCtx, ctx, 4, res)
  }
}
