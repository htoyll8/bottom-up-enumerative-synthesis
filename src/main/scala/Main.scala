package org.bottomup.arithmetic

import Arithmetic.{Bool, EndsWith, Env, Eq, ErrTy, Expr, IntTy, IsAlpha, IsDigit, Lower, Minus, Num, NumV, Plus, Str, StrReplace, StrSplit, StringTy, StringV, Times, Type, Value, Var, Variable, aryOf, canMake, eval, init, mkCode, tyOf, verify}


object Main {
  def main(args: Array[String]): Unit = {
    val vocab = Vocab(List(
      Var("x"),
      Var("sep"),
      Num(0),
      Num(1),
      Num(2),
      Num(3),
      Num(4),
    ),
      List(
        "SPLIT",
        "LOWER",
        "LENGTH",
        "INDEX",
        //"CONCAT",
        //"STRREPLACE"
      ))

    val ctx = List(
      Map(
        Var("x").x -> StringV("Tour Lour Pour Kour"),
        Var("sep").x -> StringV(" "),
      ),
      Map(
        Var("x").x -> StringV("Bell Tell Fell Sell"),
        Var("sep").x -> StringV(" "),
      ),
    )

    val res = List(
      NumV(4),
      NumV(4)
    )

    val typeCtx = Map(
      Var("x").x -> StringTy,
      Var("sep").x -> StringTy
    )

    Enumeration.enumerateLeafNodes(vocab, typeCtx, ctx)
    Enumeration.enumerate(vocab, typeCtx, ctx, 1, res)

  }
}
