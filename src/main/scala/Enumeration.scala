package org.bottomup.arithmetic

import Arithmetic.{Env, ErrTy, Expr, NumV, Type, Value, Var, Variable, aryOf, astSize, canMake, childrenAstSize, eval, init, mkCode, mkCodeMultipleCtx, tyOf, verify, verifyMultipleCtx}

import scala.collection.mutable
import scala.collection.mutable._
import scala.util.control.Breaks.{break, breakable}

class Enumeration(vocab: Vocab,
                  typeCtx: Env[Type],
                  ctx: List[Env[Value]],
                  res: List[Value],
                  LIM: Int) {
  var currLevel: Int = 0
  val currLevelDiscovered: ListBuffer[List[Value]] = ListBuffer[List[Value]]()
  val valueSpace: mutable.Map[Int, mutable.ArrayBuffer[Expr]] = mutable.Map[Int, mutable.ArrayBuffer[Expr]]()

  def isDisovered(prog: Expr): Boolean = {
    valueSpace.getOrElseUpdate(currLevel, mutable.ArrayBuffer[Expr](prog))
    valueSpace(currLevel).contains(prog)
  }

  def isPartiallyCorrect(prog: Expr): Boolean = ???

  def enumerate(): Unit = {
    while (currLevel <= LIM) {
      val candidates = newPrograms()
      while (candidates.hasNext) {
        val prog = candidates.next()
        // Evaluate program.
        val e: List[Value] = ???
        if (e == res) {
          // Add program to solution bank.
        } else if (isDisovered(prog)) {
          // Continue.
        } else if (isPartiallyCorrect(prog)) {

        }
        currLevelDiscovered += eval(ctx,prog)
      }
      currLevel += 1
    }
  }

  def newPrograms(): Iterator[Expr] = {
    for (op <- vocab.operations) {
      // Calculate cost of op.
      val opName = op._1
      val opCost = 0
      val opArity = aryOf(opName)
      if (opCost == currLevel && opArity == 0) {
        // Return leaf node.
      } else if (opCost < currLevel && opArity > 0) {
        // Create all permutations of subexpression costs.
        // Create all permutations of subexpressions.
      }
    }
    List().iterator
  }

}
