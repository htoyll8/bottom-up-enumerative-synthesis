package org.bottomup.arithmetic

import Arithmetic.{Minus, Num, Plus}
import JsonSerializer.ExprFormat

import org.scalatest.funsuite.AnyFunSuite

class JSONTests extends AnyFunSuite {
  test("Addition") {
    val addition = Plus(List(Num(2), Minus(List(Num(1), Num(5)))))
    val res = ExprFormat.writes(addition)
    assert(res == "")
  }
}
