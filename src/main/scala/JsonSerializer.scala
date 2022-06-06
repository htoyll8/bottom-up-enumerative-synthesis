package org.bottomup.arithmetic
import Arithmetic.{Bool, Expr, Index, Length, Lower, Minus, Num, Plus, Str, StrSplit, StrSubStr, Var}

import play.api.libs.json._

object JsonSerializer {
//  implicit object NumTypeFormat extends Format[Num] {
//    override def reads(json: JsValue): JsResult[Num] = for {
//      n <- (json \ "n").validate[Int]
//    } yield Num(n)
//
//    def writes(o: Num): JsValue = Json.obj(
//      "n" -> o.n.toString
//    )
//  }
//
//  implicit object StrTypeFormat extends Format[Str] {
//    override def reads(json: JsValue): JsResult[Str] = for {
//      s <- (json \ "s").validate[String]
//    } yield Str(s)
//
//    def writes(o: Str): JsValue = Json.obj(
//      "s" -> o.s
//    )
//  }
//
//  implicit object PlusTypeFormat extends Format[Plus] {
//    override def reads(json: JsValue): JsResult[Plus] = for {
//      children <- (json \ "children").validate[List[Expr]]
//    } yield Plus(children)
//
//    def writes(o: Plus): JsValue = Json.obj(
//      "children" -> o.children.toString
//    )
//  }




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
      case l: Length => Json.obj(
        "op" -> "LENGTH",
        "children" -> l.children
      )
      case l: Lower => Json.obj(
        "op" -> "LOWER",
        "children" -> l.children
      )
      case i: Index => Json.obj(
        "op" -> "INDEX",
        "children" -> i.children
      )
      case s: StrSubStr => Json.obj(
        "op" -> "STRSUBSTR",
        "children" -> s.children
      )
      case s: StrSplit => Json.obj(
        "op" -> "STRSPLIT",
        "children" -> s.children
      )
    }

    override def reads(json: JsValue): JsResult[Expr] = ???
  }
}