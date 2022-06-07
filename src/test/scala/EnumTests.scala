package org.bottomup.arithmetic
import Arithmetic.{Bool, ErrTy, Expr, Index, IntTy, Minus, Num, NumArr, NumV, Plus, Str, StrSplit, StrToList, StringArr, StringArrV, StringTy, StringV, Type, Value, Var, aryOf, canMake, eval, init, mkCode, mkCodeMultipleCtx, tyOf, verify}

import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class EnumTests extends AnyFunSuite {
  test("First four chars") {
    val vocab = Vocab(List(
      Var("str"),
      Var("sep"),
      Num(0),
      Num(4)
    ),
      mutable.Map(
        "SPLIT" -> 0,
        "STRSUBSTR" -> 0,
        "LENGTH" -> 0,
        "INDEX" -> 0
      )
    )

    val ctx = List(
      Map(
        Var("str").x -> StringV("EC1A-1BB-AC12                         "),
        Var("sep").x -> StringV("-")
      ),
      Map(
        Var("str").x -> StringV("DN55-IPT-AB34"),
        Var("sep").x -> StringV("-")
      ),
      Map(
        Var("str").x -> StringV("ASCN-1ZZ-CD89"),
        Var("sep").x -> StringV("-")
      ),
      Map(
        Var("str").x -> StringV("STHL-1ZZ-P065"),
        Var("sep").x -> StringV("-")
      )
    )

    val typeCtx = Map(
      Var("str").x -> StringTy,
      Var("sep").x -> StringTy
    )

    val res = List(
      StringV("EC1A"),
      StringV("DN55"),
      StringV("ASCN"),
      StringV("STHL")
    )

    Enumeration.enumerateLeafNodes(vocab, typeCtx, ctx)
    Enumeration.enumerate(vocab, typeCtx, ctx)
    val matches = Enumeration.computeRes(res, ctx)
    matches.foreach(e => println(mkCodeMultipleCtx(ctx, e)))
  }

  test("Barred str --> List") {
    val vocab = Vocab(List(
      Var("str"),
      Var("sep"),
      Num(1),
      Num(2),
      Str("@"),
      Str("_"),
      Str(" ")
    ),
      mutable.Map(
        "SPLIT" -> 0,
        "STRREPLACE" -> 0,
        "LENGTH" -> 0,
        "INDEX" -> 0
      )
    )

    val ctx = List(
      Map(
        Var("str").x -> StringV("jim_brown@gmail.com,Jim,Brown,33,Seattle,WA"),
        Var("sep").x -> StringV(",")
      ),
      Map(
        Var("str").x -> StringV("aya_shizuoka@aol.com,Aya,Shizuoka,27,Portland,OR"),
        Var("sep").x -> StringV(",")
      ),
      Map(
        Var("str").x -> StringV("janice_drexel@harvard.edu,Janice,Drexel,41,Atla,WY"),
        Var("sep").x -> StringV(",")
      )
    )

    val typeCtx = Map(
      Var("str").x -> StringTy,
      Var("sep").x -> StringTy
    )

    val res = List(
      StringV("Jim Brown"),
      StringV("Aya Shizuoka"),
      StringV("Janice Drexel")
    )

    Enumeration.enumerateLeafNodes(vocab, typeCtx, ctx)
    Enumeration.enumerate(vocab, typeCtx, ctx)
    val matches = Enumeration.computeRes(res, ctx)
    matches.foreach(e => println(mkCodeMultipleCtx(ctx, e)))
  }

  test("Hockey player names") {
    val vocab = Vocab(List(
      Str("m")
    ),
      mutable.Map(
        "SPLIT" -> 0
      )
    )

    val ctx = List(
      Map(
        Var("row") -> StringV("1,Women,Canada,Meghan,Agosta,148,5'7,2/12/87,Ruthven,Ont.,Forward"),
        Var("sep") -> StringV(",")
      )
    )

    val res = List(
      StringV("M. Agosta")
    )
  }
}
