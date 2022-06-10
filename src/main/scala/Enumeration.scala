package org.bottomup.arithmetic

import Arithmetic.{Env, ErrTy, Expr, NumV, Type, Value, Var, Variable, aryOf, astSize, canMake, childrenAstSize, eval, init, mkCode, mkCodeMultipleCtx, tyOf, verify, verifyMultipleCtx}

import scala.Console.in
import scala.collection.mutable
import scala.collection.mutable._
import scala.util.control.Breaks.{break, breakable}

class Enumeration(vocab: Vocab,
                  typeCtx: Env[Type],
                  ctx: List[Env[Value]],
                  res: List[Value],
                  LIM: Int) {

  /** The current level. */
  var currLevel: Int = 0

  /** Stores the programs found at each level. */
  val valueSpace: mutable.Map[Int, mutable.ArrayBuffer[Expr]] = mutable.Map[Int, mutable.ArrayBuffer[Expr]]()

  /** Stores the results of the current level. */
  val currLevelDiscovered: ListBuffer[List[Value]] = ListBuffer[List[Value]]()

  /** Stores partial solutions. */
  val partialSolutions: mutable.Map[List[Value], (Expr, Int)] = mutable.Map[List[Value], (Expr, Int)]()

  /** Stores solutions. */
  val solutions: mutable.Map[Int, mutable.ArrayBuffer[Expr]] = mutable.Map[Int, mutable.ArrayBuffer[Expr]]()

  /** Stores every production rules probability. */
  val probMap: mutable.Map[Expr, Double] = mutable.Map[Expr, Double]()

  /** Determines whether the program satisfies a subset of the examples. */
  def isPartiallyCorrect(evalRes: List[Value]): Boolean =
    (evalRes zip res).exists(l => l._1 == l._2)

  /** rounded negative log of the probability */
  def cost(prog: Expr) = ???

  /** Find examples the program satisfies. */
  def p(prog: Expr, evalRes: List[Value]) = {
    val examplesSatisfied = res.intersect(evalRes)
    if (examplesSatisfied == evalRes) {
      // Add solution to current level bank.
      val levelSolution = solutions.getOrElseUpdate(currLevel,mutable.ArrayBuffer[Expr]())
      levelSolution += prog
    } else {
      if (partialSolutions.contains(examplesSatisfied)) {
        // Check if program is cheaper.
      } else {
        partialSolutions(examplesSatisfied) = (prog, currLevel)
      }
    }
  }

  def isDisovered(prog: Expr): Boolean = {
    if (!valueSpace.contains(currLevel)) {
      valueSpace(currLevel) = mutable.ArrayBuffer[Expr](prog)
    }
    valueSpace(currLevel).contains(prog)
  }

  def enumerate(): Unit = {
    while (currLevel <= LIM) {
      val candidates = newPrograms()
      while (candidates.hasNext) {
        val prog = candidates.next()
        // Evaluate program.
        val e: List[Value] = eval(ctx,prog)
        if (e == res) {
          // Add program to solution bank.
        } else if (isDisovered(prog)) {
          // Continue.
        } else if (isPartiallyCorrect(e)) {
          //
        }
        currLevelDiscovered += e
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
