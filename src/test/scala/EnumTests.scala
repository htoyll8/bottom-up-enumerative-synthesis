package org.bottomup.arithmetic
import Arithmetic.{Bool, ErrTy, Expr, Index, IntTy, Minus, Num, NumArr, NumV, Plus, Str, StrSplit, StrToList, StringArr, StringArrV, StringTy, StringV, Type, Value, Var, aryOf, canMake, eval, init, mkCode, postprocess, tyOf, verify}

import org.bottomup.arithmetic.Enumeration.closeFile
import org.bottomup.arithmetic.JsonSerializer.ExprFormat
import org.scalatest.funsuite.AnyFunSuite

class EnumTests extends AnyFunSuite {
  test("Find 5") {
    val vocab = Vocab(List(
      Num(1),
      Num(2),
      Num(3)
    ),
      List(
      "PLUS",
      "MINUS"
    ))

    Enumeration.enumerateLeafNodes(vocab, Map.empty, Map.empty)
    Enumeration.enumerateNodes(vocab, Map.empty, Map.empty, 60)
    closeFile()
  }
}
