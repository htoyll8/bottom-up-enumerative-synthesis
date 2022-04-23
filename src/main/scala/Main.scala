package org.bottomup.arithmetic

import Arithmetic.{Env, Expr, Num, Plus, Value, Variable, eval}

object Main {
  def main(args: Array[String]): Unit = {
    val add: Expr = Plus(Num(1), Num(2))

    def evaluate(ast: Expr):Value =
      eval(Map.empty,ast)

    println(evaluate(add))
  }
}
