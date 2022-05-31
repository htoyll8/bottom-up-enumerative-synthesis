package org.bottomup.arithmetic

import Arithmetic.{Bool, EndsWith, Env, Eq, ErrTy, Expr, IntTy, IsAlpha, IsDigit, Lower, Minus, Num, NumV, Plus, Str, StrReplace, StrSplit, StringV, Times, Type, Value, Var, Variable, aryOf, eval, init, mkCode, tyOf, verify}

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}

object Main {
  def main(args: Array[String]): Unit = {
    val leafNodes = List(
      Var("x"),
      Var("y"),
      Str("Tyler"),
      Str("Holloway")
    )
    val grammar = List(
      "LENGTH",
      "EQ",
      "ISALPHA",
      "PLUS",
      "LOWER",
      "UPPER",
      "INDEX",
      "IFTHENELSE",
      "STRREPLACE",
      "ENDSWITH",
      "CONCAT",
      "MINUS",
      "TIMES",
    )

    val ctx = Map(
      Var("x").x -> NumV(3),
      Var("y").x -> NumV(2),
    )

    val typeCtx = Map(
      Var("x").x -> IntTy,
      Var("y").x -> IntTy,
    )

    def typecheck(ast: Expr):Type =
      tyOf(Map.empty,ast);

    def evaluate(ast: Expr):Value =
      eval(ctx, ast)

    /* Enumeration */
    val prevLevelDiscovered = ListBuffer[Expr]()
    val currentLevelDiscovered = ListBuffer[Expr]()

    def enumerateLeafNodes(): Unit = {
      grammar.foreach(exprStr => {
        val childrenCandidates = (leafNodes ++ leafNodes).combinations(aryOf(exprStr))
        while (childrenCandidates.hasNext) {
          val params = childrenCandidates.next()
          val ast = init(exprStr, params)
          if (tyOf(typeCtx, ast) != ErrTy) prevLevelDiscovered += ast
        }
      })
    }

    def enumerateNodes(): Unit = {
      val plist = prevLevelDiscovered
      var counter = 0
      for( i <- 1 to 7){
        breakable {
          grammar.foreach(exprStr => {
            val childrenCandidates = (plist ++ plist).combinations(aryOf(exprStr))
            while (childrenCandidates.hasNext) {
              val params = childrenCandidates.next()
              val ast = init(exprStr, params.toList)
              if (tyOf(typeCtx,ast) != ErrTy && verify(ctx,typeCtx,ast)) {
                if (exprStr == "SPLIT") println(ast)
                currentLevelDiscovered += ast
                counter += 1
              }
              if (counter == 200000) {
                println("Breaking...")
                break
              }
            }
          })
        }
        println("Round: " + i)
        prevLevelDiscovered ++= currentLevelDiscovered
        currentLevelDiscovered.clear()
      }
    }

    enumerateLeafNodes()
    enumerateNodes()
    // prevLevelDiscovered.foreach(ast => println(mkCode(ctx,ast)))

    // 5
//    val res = prevLevelDiscovered.map(ast => {
//      if (evaluate(ast) == NumV(5)) mkCode(ctx,ast)
//    }).toSet

    // HelloHello
    val res = prevLevelDiscovered.map(ast => {
      if (evaluate(ast) == StringV("tyler")) mkCode(ctx,ast)
    }).toSet


    res.foreach(r => {
      println(r)
      println
    })
  }
}
