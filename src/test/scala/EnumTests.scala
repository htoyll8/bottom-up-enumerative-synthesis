package org.bottomup.arithmetic
import Arithmetic.{Bool, ErrTy, Expr, Index, IntTy, Minus, Num, NumArr, NumV, Plus, Str, StrSplit, StrToList, StringArr, StringArrV, StringTy, StringV, Type, Value, Var, aryOf, canMake, eval, init, mkCode, tyOf, verify}

import org.bottomup.arithmetic.Enumeration.closeFile
import org.bottomup.arithmetic.JsonSerializer.ExprFormat
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable

class EnumTests extends AnyFunSuite {
  test("Find 5") {
    val vocab = Vocab(List(
      Var("fname"),
      Str("")
    ),
      mutable.Map(
        "LENGTH" -> 0.7,
        "STRTOLIST" -> 0.65,
        "ARRJOIN" -> 0.55,
        "LOWER" -> 0.45,
        //"UPPER" -> 0.45,
    ))

    val ctx = List(
      Map(
        Var("fname").x -> StringV("Tyler"),
      )
    )

    val typeCtx = Map(
      Var("fname").x -> StringTy,
    )

    val res = List(
      NumV(5)
    )

    Enumeration.enumerateLeafNodes(vocab, typeCtx, ctx)
    Enumeration.enumerate(vocab, typeCtx, ctx, 2, res)
  }

  test("Find 4 and 3") {
    val vocab = Vocab(List(
      Var("x"),
      Var("sep"),
      Num(0),
      Num(1),
      Num(2),
      Num(3),
      Num(4),
    ),
      mutable.Map(
        "SPLIT" -> 0.4,
        "LOWER" -> 0.15,
        "UPPER" -> 0.15,
        "CONCAT" -> 0.25,
        "STRTOLIST" -> 0.35,
        "LENGTH" -> 0.5,
        "INDEX" -> 0.5,
      )
    )

    val ctx = List(
      Map(
        Var("x").x -> StringV("Tour Lour Pour Kour"),
        Var("sep").x -> StringV(" "),
      ),
      Map(
        Var("x").x -> StringV("Bel Tel Fel"),
        Var("sep").x -> StringV(" "),
      ),
    )

    val res = List(
      NumV(4),
      NumV(3)
    )

    val typeCtx = Map(
      Var("x").x -> StringTy,
      Var("sep").x -> StringTy
    )

    Enumeration.enumerateLeafNodes(vocab, typeCtx, ctx)
    Enumeration.enumerate(vocab, typeCtx, ctx, 1, res)
  }

  test("Find 4") {
    val vocab = Vocab(List(
      Var("x"),
      Var("sep"),
      Num(0),
      Num(1),
      Num(2),
      Num(3),
      Num(4),
    ),
      mutable.Map(
        "SPLIT" -> 1,
        "INDEX" -> 0,
        "LOWER" -> 0,
        "LENGTH" -> 0,
        //"STRSUBSTR" -> 0,
        //"UPPER" -> 0.15,
        //"CONCAT" -> 0.25,
//      "STRTOLIST" -> 0.35,
//      "ARRJOIN" -> 0.3,
      )
    )

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

  test("Find last name") {
    val vocab = Vocab(List(
      Var("name"),
      Str(" "),
      Num(0),
      Num(1),
    ),
      mutable.Map(
        "LENGTH" -> 0.7,
        "SPLIT" -> 0.8,
        "INDEX" -> 0.9,
        "STRTOLIST" -> 0.65,
        "ARRJOIN" -> 0.55,
      ))

    val ctx = List(
      Map(
        Var("name").x -> StringV("Tyler Hollo"),
      )
    )

    val typeCtx = Map(
      Var("name").x -> StringTy,
    )

    val res = List(
      StringV("Hollo")
    )

    Enumeration.enumerateLeafNodes(vocab, typeCtx, ctx)
    Enumeration.enumerate(vocab, typeCtx, ctx, 3, res)
  }


  test("Find date") {
    val vocab = Vocab(List(
      Var("date"),
      Var("n"),
      Str("-"),
      Num(1)
    ),
      mutable.Map(
        "LENGTH" -> 0.7,
        "SPLIT" -> 0.8,
        "INDEX" -> 0.9,
        "MINUS" -> 0.55,
        "STRTOLIST" -> 0.65,
        "ARRJOIN" -> 0.55,
      ))

    val ctx = List(
      Map(
        Var("date").x -> StringV("1/17/16-1/18/17"),
        Var("n").x -> NumV(1),
      )
    )

    val typeCtx = Map(
      Var("date").x -> StringTy,
      Var("n").x -> IntTy,
    )

    val res = List(
      StringV("1/17/16")
    )

    Enumeration.enumerateLeafNodes(vocab, typeCtx, ctx)
    Enumeration.enumerate(vocab, typeCtx, ctx, 3, res)
  }

  test("Remove angles") {
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

  test("Phone number") {
    val vocab = Vocab(List(
      Var("arg"),
      Num(4),
      Num(5)
    ),
      mutable.Map(
        "CONCAT" -> 0.7,
        "REPLACE" -> 0.8,
      ))

    val ctx = List(
      Map(
        Var("arg").x -> StringV("+95 310-537-401")
      ),
      Map(
        Var("arg").x -> StringV("+72 001-050-856")
      ),
      Map(
        Var("arg").x -> StringV("+106 769-858-438")
      )
    )

    val typeCtx = Map(
      Var("arg").x -> StringTy,
    )

    val res = List(
      StringV("310"),
      StringV("001"),
      StringV("769")
    )

    Enumeration.enumerateLeafNodes(vocab, typeCtx, ctx)
    Enumeration.enumerate(vocab, typeCtx, ctx, 3, res)
  }

  //TODO: count.
  test("Newlines") {
    val vocab = Vocab(List(
      Var("arg"),
      Str("\n"),
    ),
      mutable.Map(
        "SPLIT" -> 0.7,
        "LENGTH" -> 0.6
      ))

    val ctx = List(
      Map(
        Var("arg").x -> StringV("one")
      ),
      Map(
        Var("arg").x -> StringV("one\\ntwo")
      ),
      Map(
        Var("arg").x -> StringV("one\\ntwo\\nthree")
      ),
      Map(
        Var("arg").x -> StringV("one\\ntwo\\nthree\\four")
      )
    )

    val typeCtx = Map(
      Var("arg").x -> StringTy,
    )

    val res = List(
      NumV(0),
      NumV(1),
      NumV(2),
      NumV(3),
    )

    Enumeration.enumerateLeafNodes(vocab, typeCtx, ctx)
    Enumeration.enumerate(vocab, typeCtx, ctx, 3, res)
  }
}
