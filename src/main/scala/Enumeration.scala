package org.bottomup.arithmetic

import Arithmetic.{Env, ErrTy, Expr, NumV, Type, Value, Var, Variable, aryOf, astSize, canMake, eval, evalMultipleCtx, init, mkCode, mkCodeMultipleCtx, tyOf, verify, verifyMultipleCtx}

import scala.collection.mutable
import scala.collection.mutable._

object Enumeration {
  val prevLevelDiscovered: ListBuffer[Expr] = ListBuffer[Expr]()
  val currLevelDiscovered: ListBuffer[Expr] = ListBuffer[Expr]()
  val valueSpace: mutable.Set[Expr] = mutable.Set[Expr]()

  def initPrevLevelDiscovered(leafNodes: List[Expr]): Unit = prevLevelDiscovered ++= leafNodes

  def sortExprBySize(exprs: List[Expr]): List[Expr] = exprs.sortBy(astSize)

  def sortOpByWeight(ops: mutable.Map[String, Double]): Predef.Map[String, Double] =
    ops.toList.sortBy(_._2).toMap

  def cartesianProduct(exprs: List[Expr], n: Int): Iterator[List[Expr]] =
    exprs.combinations(n).flatMap(_.permutations)

  def filterCandidates(candidates: Iterator[List[Expr]], typeCtx: Env[Type], opStr: String): Iterator[List[Expr]] = {
    def typecheck(ast: Expr):Type = tyOf(typeCtx,ast)
    candidates.filter(c => canMake(opStr, c.map(typecheck)))
  }

  def verifyAndAddChild(level: ListBuffer[Expr], ctx: List[Env[Value]], typeCtx: Env[Type], expr: Expr): Unit =
    if (tyOf(typeCtx, expr) != ErrTy && verifyMultipleCtx(ctx,typeCtx,expr)) level += expr

  def computeRes(res: List[Value], ctx: List[Env[Value]]): List[Expr] =
    valueSpace.filter(e => evalMultipleCtx(ctx, e) == res).toList.sortBy(astSize)

  def changeRootLevel(): Unit = {
    prevLevelDiscovered ++= currLevelDiscovered
    currLevelDiscovered.clear()
  }

  def changeLevel(): Unit = {
    valueSpace ++= prevLevelDiscovered
    prevLevelDiscovered.clear()
    prevLevelDiscovered ++= currLevelDiscovered
    currLevelDiscovered.clear()
  }

  def enumerateUtil(grammar: Vocab,
                    typeCtx: Env[Type],
                    ctx: List[Env[Value]]): Unit = {
    val operations = sortOpByWeight(grammar.operations)
    for ((opStr, weight) <- operations) {
      val childrenCandidates = cartesianProduct(prevLevelDiscovered.toList, aryOf(opStr))
      val children = filterCandidates(childrenCandidates, typeCtx, opStr)
      println("Got children " + opStr)
      while(children.hasNext) {
        val params = children.next()
        val ast = init(opStr, params)
        verifyAndAddChild(currLevelDiscovered, ctx, typeCtx, ast)
      }
    }
  }

  def enumerateLeafNodes(grammar: Vocab, typeCtx: Env[Type], ctx: List[Env[Value]]): Unit = {
    initPrevLevelDiscovered(grammar.leafNodes)
    enumerateUtil(grammar, typeCtx, ctx)
    changeRootLevel()
  }

  def enumerate(grammar: Vocab, typeCtx: Env[Type], ctx: List[Env[Value]]): Unit = {
    for (_ <- 0 to 2) {
      enumerateUtil(grammar, typeCtx, ctx)
      changeLevel()
    }
    valueSpace.toList.sortBy(astSize).foreach(e => println(/*mkCodeMultipleCtx(ctx, e),*/ evalMultipleCtx(ctx, e)))
  }
}
