package org.bottomup.arithmetic

import Arithmetic.{Env, ErrTy, Expr, NumV, Type, Value, Var, Variable, aryOf, astSize, canMake, eval, evalMultipleCtx, init, mkCode, mkCodeMultipleCtx, tyOf, verify, verifyMultipleCtx}

import org.bottomup.arithmetic.JsonSerializer._
import play.api.libs.json.JsObject

import scala.concurrent.duration.DurationInt
import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable
import scala.collection.mutable._
import scala.util.control.Breaks.{break, breakable}

object Enumeration {
  val file = new File("file_1.txt")
  val bw = new BufferedWriter(new FileWriter(file))
  val prevLevelDiscovered: ListBuffer[Expr] = ListBuffer[Expr]()
  val currentLevelDiscovered: ListBuffer[Expr] = ListBuffer[Expr]()
  val valueSpace = mutable.Set[Expr]()

  def enumerateLeafNodes(grammar: Vocab, typeCtx: Env[Type], ctx: List[Env[Value]]): Unit = {
    val leafNodes = grammar.leafNodes
    val operations = grammar.operations
    prevLevelDiscovered ++= leafNodes
    def typecheck(ast: Expr):Type = tyOf(typeCtx,ast)
    operations.foreach(exprStr => {
      val childrenCandidates = combinationList(leafNodes, aryOf(exprStr))
      val children = childrenCandidates.filter(c => canMake(exprStr, c.map(typecheck)))
      while (children.hasNext) {
        val params = children.next()
        val ast = init(exprStr, params)
        if (tyOf(typeCtx, ast) != ErrTy && verifyMultipleCtx(ctx,typeCtx,ast)) prevLevelDiscovered += ast
      }
    })
  }

  def enumerate(grammar: Vocab, typeCtx: Env[Type], ctx: List[Env[Value]], timeout: Int, res: List[Value]): Unit = {
    val operations = grammar.operations
    val deadline = timeout.seconds.fromNow

    def typecheck(ast: Expr):Type = tyOf(typeCtx,ast)

    def computeRes() = {
      val matches = valueSpace.filter(e => evalMultipleCtx(ctx, e) == res).toList.sortBy(astSize)
      //matches.foreach(m => println(m, astSize(m)))
      matches.foreach(e => println(mkCodeMultipleCtx(ctx, e)))
    }

    while(deadline.hasTimeLeft) {
      var i = 0
      breakable {
        val plist = prevLevelDiscovered.sortBy(astSize)
        operations.foreach(exprStr => {
          val childrenCandidates: List[List[Expr]] = combinationList(plist.toList, aryOf(exprStr)).toList
          val children = childrenCandidates.filter(c => {
            val childrenCandidateTypes = c.map(typecheck)
            canMake(exprStr, childrenCandidateTypes)
          })

          println("length: " + children.length)
          val childrenIterator = children.iterator

          while (childrenIterator.hasNext) {
            val params = childrenIterator.next()
            val ast = init(exprStr, params)
            if (tyOf(typeCtx, ast) != ErrTy && verifyMultipleCtx(ctx,typeCtx,ast)) currentLevelDiscovered += ast
          }
        })

        i += 1

        // Change level
        valueSpace ++= prevLevelDiscovered
        prevLevelDiscovered.clear()
        prevLevelDiscovered ++= currentLevelDiscovered
        currentLevelDiscovered.clear()
      }
    }
    valueSpace ++= currentLevelDiscovered
    computeRes()
    closeFile()
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
