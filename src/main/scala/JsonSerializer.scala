package org.bottomup.arithmetic
import Arithmetic.{Bool, Expr, Minus, Num, Plus, Str, Var}

import play.api.libs.json._

object JsonSerializer {
  implicit object ExprFormat extends Format[Expr] {
    override def writes(o: Expr): JsObject = o match {
      case n: Num => Json.obj(
        "op" -> "NUM",
        "n" -> n.n.asInstanceOf[Int]
      )
      case s: Str => Json.obj(
        "op" -> "STR",
        "s" -> s.s
      )
      case b: Bool => Json.obj(
        "op" -> "BOOL",
        "n" -> b.n
      )
      case v: Var => Json.obj(
        "op" -> "VAR",
        "x" -> v.x
      )
      case p: Plus => Json.obj(
        "op" -> "PLUS",
        "children" -> p.children
      )
      case m: Minus => Json.obj(
        "op" -> "MINUS",
        "children" -> m.children
      )
    }

    override def reads(json: JsValue): JsResult[Expr] = ???
  }
}