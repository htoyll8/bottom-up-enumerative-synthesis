package org.bottomup.arithmetic

import Arithmetic.{Env, ErrTy, Expr, Type, Value, aryOf, canMake, init, tyOf, verify}

import org.bottomup.arithmetic.JsonSerializer.ExprFormat
import play.api.libs.json.JsObject

import scala.concurrent.duration.DurationInt
import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable._
import scala.util.control.Breaks.{break, breakable}

object Enumeration {
  val file = new File("file_1.txt")
  val bw = new BufferedWriter(new FileWriter(file))
  val prevLevelDiscovered: ListBuffer[Expr] = ListBuffer[Expr]()
  val currentLevelDiscovered: ListBuffer[Expr] = ListBuffer[Expr]()

  def enumerateLeafNodes(grammar: Vocab, typeCtx: Env[Type], ctx: Env[Value]): Unit = {
    val leafNodes = grammar.leafNodes
    val operations = grammar.operations
    def typecheck(ast: Expr):Type = tyOf(typeCtx,ast)
    operations.foreach(exprStr => {
      val childrenCandidates = combinationList(leafNodes, aryOf(exprStr))
      val children = childrenCandidates.filter(c => canMake(exprStr, c.map(typecheck)))
      while (children.hasNext) {
        val params = children.next()
        val ast = init(exprStr, params)
        if (tyOf(typeCtx, ast) != ErrTy && verify(ctx,typeCtx,ast)) prevLevelDiscovered += ast
      }
    })
  }

  def enumerate(grammar: Vocab, typeCtx: Env[Type], ctx: Env[Value], timeout: Int): Unit = {
    val operations = grammar.operations
    val deadline = timeout.seconds.fromNow
    def typecheck(ast: Expr):Type = tyOf(typeCtx,ast)
    while(deadline.hasTimeLeft) {
      operations.foreach(exprStr => {
        val childrenCandidates: List[List[Expr]] = combinationList(prevLevelDiscovered.toList, aryOf(exprStr)).toList
        val children = childrenCandidates.filter(c => {
          val childrenCandidateTypes = c.map(typecheck)
          canMake(exprStr, childrenCandidateTypes)
        }).iterator
        while (children.hasNext) {
          val params = children.next()
          val ast = init(exprStr, params)
          if (tyOf(typeCtx, ast) != ErrTy && verify(ctx,typeCtx,ast)) currentLevelDiscovered += ast
        }
      })
      // Change level
      flushLevel(prevLevelDiscovered)
      prevLevelDiscovered ++= currentLevelDiscovered
      currentLevelDiscovered.clear()
    }
    flushLevel(currentLevelDiscovered)
  }

  def writeJson(exprs: List[Expr]): List[JsObject] = exprs.map(ExprFormat.writes)

  def writeFile(s: String): Unit = bw.write(s+"\n")

  def closeFile(): Unit = bw.close()

  def combinationList(ls: List[Expr], arity: Int): Iterator[List[Expr]] =
    ls.combinations(arity).flatMap(c => c.permutations)

  def flushLevel(level: ListBuffer[Expr]): Unit = {
    for (expr <-level) writeFile(ExprFormat.writes(expr).toString)
    level.clear()
  }
}
