package org.bottomup.arithmetic
import Arithmetic.{Expr, Minus, Plus}
import play.api.libs.json._

//object JsonSerializer {
//  val fmtPlus     = Json.format[Plus]
//  val fmtMinus     = Json.format[Minus]
//
//  implicit object ExprFormat extends Format[Expr] {
//    override def writes(o: Expr): JsValue = o match {
//      case p: Plus =>
//        Json.obj("Expr" ->
//          Json.obj("Plus" ->
//            Json.toJson(p)(fmtPlus)))
//      case s: Minus =>
//        Json.obj("Expr" ->
//          Json.obj("Minus" ->
//            Json.toJson(s)(fmtMinus)))
//    }
//    override def reads(json: JsValue): JsResult[Expr] = ???
//  }
//}
