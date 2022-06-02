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

  def writeJson(exprs: List[Expr]): List[JsObject] = exprs.map(ExprFormat.writes)

  def writeFile(s: String): Unit = bw.write(s+"\n")

  def closeFile(): Unit = bw.close()

  def combinationList(ls: List[Expr], arity: Int) =
    ls.combinations(arity).flatMap(c => c.permutations)

  def flushPreviousLevel(): Unit = {
    for (expr <- prevLevelDiscovered) writeFile(ExprFormat.writes(expr).toString)
    prevLevelDiscovered.clear()
  }

  def flushCurrentLevel(): Unit = {
    for (expr <- currentLevelDiscovered) writeFile(ExprFormat.writes(expr).toString)
    currentLevelDiscovered.clear()
  }

  def changeLevel(): Unit = {
    flushPreviousLevel()
    prevLevelDiscovered ++= currentLevelDiscovered
    currentLevelDiscovered.clear()
  }

  def computeCurrentLevel(childrenCandidates: Iterator[List[Expr]],
                          outputLst: ListBuffer[Expr],
                          exprStr: String,
                          typeCtx: Env[Type],
                          ctx: Env[Value]) = {
    while (childrenCandidates.hasNext) {
      val params = childrenCandidates.next()
      val ast = init(exprStr, params)
      if (tyOf(typeCtx, ast) != ErrTy && verify(ctx,typeCtx,ast)) outputLst += ast
    }
  }

  def enumerateLeafNodes(grammar: Vocab, typeCtx: Env[Type], ctx: Env[Value]): Unit = {
    val leafNodes = grammar.leafNodes
    val operations = grammar.operations
    operations.foreach(exprStr => {
      val childrenCandidates = (leafNodes ++ leafNodes).combinations(aryOf(exprStr))
      computeCurrentLevel(childrenCandidates, prevLevelDiscovered, exprStr, typeCtx, ctx)
    })
  }

  def enumerateNodes(grammar: Vocab,
                     typeCtx: Env[Type],
                     ctx: Env[Value],
                     timeout: Int): Unit = {
    val plist = prevLevelDiscovered
    val operations = grammar.operations
    val deadline = timeout.seconds.fromNow

    def typecheck(ast: Expr):Type =
      tyOf(typeCtx,ast)

    breakable {
      operations.foreach(exprStr => {
        if (!deadline.hasTimeLeft) {
          flushCurrentLevel()
          break()
        }
        val childrenCandidates: List[List[Expr]] = combinationList(plist.toList, aryOf(exprStr)).toList
        val children = childrenCandidates.filter(c => canMake(exprStr, c.map(typecheck))).toIterator
        computeCurrentLevel(children, currentLevelDiscovered, exprStr, typeCtx, ctx)
      })
      changeLevel()
    }
  }
}
