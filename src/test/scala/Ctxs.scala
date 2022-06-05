package org.bottomup.arithmetic

import Arithmetic._
import Enumeration.closeFile

import org.scalatest.funsuite.AnyFunSuite

class Ctxs extends AnyFunSuite {
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
      List(
        "SPLIT",
        "INDEX",
        "LENGTH",
        "CONCAT"
      ))

    val ctx = Map(
      Var("x").x -> StringV("Tour Lour Pour Kour"),
      Var("sep").x -> StringV(" "),
    )

    val typeCtx = Map(
      Var("x").x -> StringTy,
      Var("sep").x -> StringTy
    )
  }

  test("Find email") {
    val vocab = Vocab(List(
      Var("fname"),
      Var("lname"),
      Num(3),
    ),
      List(
        "CONCAT",
        "SPLIT",
        "INDEX",
        "LENGTH",
      ))

    val ctx = Map(
      Var("fname").x -> StringV("mug"),
      Var("lname").x -> StringV("pug"),
    )

    val typeCtx = Map(
      Var("fname").x -> StringTy,
      Var("lname").x -> StringTy
    )
  }
}
