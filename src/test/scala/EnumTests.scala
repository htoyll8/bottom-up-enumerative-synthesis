package org.bottomup.arithmetic
import Arithmetic.{Bool, Concat, ErrTy, Expr, Index, IntTy, Minus, Num, NumArr, NumV, Plus, Str, StrIndex, StrSplit, StrToList, StringArr, StringArrTy, StringArrV, StringTy, StringV, Type, Value, Var, aryOf, astSize, eval, init, mkCode, mkCodeMultipleCtx, tyOf, verify}

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
        "STRINDEX" -> 0,
        "ARRINDEX" -> 0
      )
    )

    val ctx = List(
      Map(
        Var("str").x -> StringV("EC1A-1BB-AC12"),
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

    val enumerator = new Enumeration(vocab, typeCtx, ctx, res, 14)
    enumerator.initProgramBank()
    enumerator.enumerate()
    enumerator.computeResults().foreach(e => println(mkCodeMultipleCtx(ctx, e)))
  }

  test("Barred str --> List") {
    val vocab = Vocab(List(
      Var("str"),
      //Var("sep"),
      Num(0),
      Num(1),
      Num(2),
      Num(8),
      Str(" "),
      Str("@")
    ),
      mutable.Map(
        "CONCAT" -> 0,
        "SPLIT" -> 0,
        "STRSUBSTR" -> 0,
        "STRREPLACE" -> 0,
        "LENGTH" -> 0,
        "TITLE" -> 0,
        "STRINDEX" -> 0,
        "ARRINDEX" -> 0
      )
    )

    val ctx = List(
      Map(
        Var("str").x -> StringArrV(ListBuffer("jim_brown@gmail.com","Jim","Brown","33","Seattle,WA")),
        Var("sep").x -> StringV(",")
      ),
      Map(
        Var("str").x -> StringArrV(ListBuffer("aya_shizuoka@aol.com","Aya","Shizuoka","27","Portland,OR")),
        Var("sep").x -> StringV(",")
      ),
      Map(
        Var("str").x -> StringArrV(ListBuffer("janice_drexel@harvard.edu","Janice","Drexel","41","Atla,WY")),
        Var("sep").x -> StringV(",")
      )
    )

    val typeCtx = Map(
      Var("str").x -> StringArrTy,
      Var("sep").x -> StringTy
    )

    val res = List(
      StringV("Jim Brown"),
      StringV("Aya Shizuoka"),
      StringV("Janice Drexel")
    )

    val enumerator = new Enumeration(vocab, typeCtx, ctx, res, 13)
    enumerator.initProgramBank()
    enumerator.enumerate()
    enumerator.valueSpace.flatMap(_._2).foreach(e => println(eval(ctx, e)))
    //enumerator.valueSpace.flatMap(_._2).foreach(e => println(mkCodeMultipleCtx(ctx, e)))
    //enumerator.computeResults().foreach(e => println(mkCodeMultipleCtx(ctx, e)))
  }

  test("Shorten thousand dollars") {
    val vocab = Vocab(List(
      Var("amount"),
      Var("sep"),
      Str("k"),
      Num(0),
      Num(3)
    ),
      mutable.Map(
        "SPLIT" -> 0,
        "INDEX" -> 0,
        "CONCAT" -> 0,
      )
    )

    val ctx = List(
      Map(
        Var("amount").x -> StringV("80,000"),
        Var("sep").x -> StringV(",")
      )
    )

    val typeCtx = Map(
      Var("amount").x -> StringTy,
      Var("sep").x -> StringTy
    )

    val res = List(
      StringV("80k")
    )

    val enumerator = new Enumeration(vocab, typeCtx, ctx, res, 5)
    enumerator.enumerate()
  }

  test("Email name") {
    val vocab = Vocab(List(
      Var("row"),
      Num(0),
      Num(1),
      Num(2),
      Str("@")
    ),
      mutable.Map(
        "SPLIT" -> 0,
        "STRSUBSTR" -> 0,
        "CONCAT" -> 0,
        "LOWER" -> 0,
        "LENGTH" -> 0,
        "STRINDEX" -> 0,
        "ARRINDEX" -> 0
      )
    )

    val ctx = List(
      Map(
        Var("row").x -> StringArrV(ListBuffer("Amy","Chang","achang@gmail.com"))
      ),
      Map(
        Var("row").x -> StringArrV(ListBuffer("Bobby","Smith","bsmith@aol.com"))
      ),
      Map(
        Var("row").x -> StringArrV(ListBuffer("Thomas","Payne","tpayne@hotmail.com"))
      )
    )

    val typeCtx = Map(
      Var("row").x -> StringArrTy
    )

    val res = List(
      StringV("achang"),
      StringV("bsmith"),
      StringV("tpayne")
    )

    val enumerator = new Enumeration(vocab, typeCtx, ctx, res, 13)
    enumerator.initProgramBank()
    enumerator.enumerate()
    enumerator.valueSpace.flatMap(_._2).foreach(e => println(mkCodeMultipleCtx(ctx, e)))
    enumerator.computeResults().foreach(e => println(mkCodeMultipleCtx(ctx, e)))
  }
}
