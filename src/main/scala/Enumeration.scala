package org.bottomup.arithmetic

import Arithmetic.{Env, ErrTy, Expr, NumV, Type, Value, Var, Variable, aryOf, astSize, canMake, childrenAstSize, eval, evalMultipleCtx, init, mkCode, mkCodeMultipleCtx, tyOf, verify, verifyMultipleCtx}

import scala.collection.mutable
import scala.collection.mutable._
import scala.util.control.Breaks.{break, breakable}

class Enumeration(vocab: Vocab, typeCtx: Env[Type], ctx: List[Env[Value]], LIM: Int) {
  val prevLevelDiscovered: ListBuffer[Expr] = ListBuffer[Expr]()
  val currLevelDiscovered: ListBuffer[Expr] = ListBuffer[Expr]()
  val valueSpace: mutable.Set[Expr] = mutable.Set[Expr]()

  def initPrevLevelDiscovered(leafNodes: List[Expr]): Unit = prevLevelDiscovered ++= leafNodes

  def sortExprBySize(exprs: List[Expr]): List[Expr] = exprs.sortBy(astSize)

  def sortOpByWeight(ops: mutable.Map[String, Double]): Predef.Map[String, Double] =
    ops.toList.sortBy(_._2).toMap

  def getChildren(exprs: List[Expr], n: Int): Iterator[List[Expr]] =
    cartesianProduct(exprs, n).toList.sortBy(childrenAstSize).iterator

  def cartesianProduct(exprs: List[Expr], n: Int): Iterator[List[Expr]] =
    exprs.combinations(n).flatMap(_.permutations)

  def filterCandidates(candidates: Iterator[List[Expr]], opStr: String): Iterator[List[Expr]] = {
    def typecheck(ast: Expr):Type = tyOf(typeCtx,ast)
    candidates.filter(c => canMake(opStr, c.map(typecheck)))
  }

  def verifyAndAddChild(level: ListBuffer[Expr], expr: Expr): Unit =
    if (tyOf(typeCtx, expr) != ErrTy && verifyMultipleCtx(ctx,typeCtx,expr)) level += expr

  def computeRes(res: List[Value]): List[Expr] =
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

  def enumerateUtil(): Unit = {
    val operations = sortOpByWeight(vocab.operations)
    for ((opStr, weight) <- operations) {
      val childrenCandidates = getChildren(prevLevelDiscovered.toList, aryOf(opStr))
      val children = filterCandidates(childrenCandidates, opStr)
      breakable {
        while(children.hasNext) {
          val params = children.next()
          val ast = init(opStr, params)
          verifyAndAddChild(currLevelDiscovered, ast)
        }
      }
    }
  }

  def enumerateLeafNodes(): Unit = {
    initPrevLevelDiscovered(vocab.leafNodes)
    enumerateUtil()
    changeRootLevel()
  }

  def enumerate(): Unit = {
    for (_ <- 0 to 2) {
      enumerateUtil()
      changeLevel()
    }
  }
}
