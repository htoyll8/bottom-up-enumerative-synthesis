package org.bottomup.arithmetic

import Arithmetic.{Env, ErrTy, Expr, Index, Length, Num, NumV, Str, StrSplit, StrSubStr, Type, Value, Var, Variable, aryOf, astSize, childTypes, childrenAstSize, eval, init, mkCode, mkCodeMultipleCtx, tyOf, verify, verifyMultipleCtx}

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
  var currLevel: Int = 1

  /** Stores the programs found at each level. */
  val valueSpace: mutable.Map[Int, mutable.ArrayBuffer[Expr]] = mutable.Map[Int, mutable.ArrayBuffer[Expr]]()

  /** Stores the results of the current level. */
  val currLevelDiscovered: ListBuffer[List[Value]] = ListBuffer[List[Value]]()

  /** Stores partial solutions. */
  val partialSolutions: mutable.Map[List[Value], (Expr, Int)] = mutable.Map[List[Value], (Expr, Int)]()

  /** Stores solutions. */
  val solutions: mutable.Map[Int, mutable.ArrayBuffer[Expr]] = mutable.Map[Int, mutable.ArrayBuffer[Expr]]()

  /** Stores every production rules probability. */
  val probMap: mutable.Map[Class[_], Double] = mutable.Map[Class[_], Double]()

  /** Compute type of expr. */
  def typecheck(expr: Expr):Type = tyOf(typeCtx,expr)

  /** Determines whether the program satisfies a subset of the examples. */
  def isPartiallyCorrect(evalRes: List[Value]): Boolean = (evalRes zip res).exists(l => l._1 == l._2)

  /** Add leaf nodes to the initial level of program bank.  */
  def initProgramBank(): Unit =  {
    valueSpace(currLevel) = ArrayBuffer[Expr]() ++ vocab.leafNodes
    currLevel += 1
  }

  /** Cartesian product.  */
  def cartesianProduct(x: List[List[Expr]]): List[List[Expr]] = x match {
    case Nil    => List(Nil)
    case h :: _ => h.flatMap(i => cartesianProduct(x.tail).map(i :: _))
  }

  /** The rounded negative log of the probability */
  def costMap(expr: Expr): Int = expr match {
    case _: Num => 1
    case _: Str => 1
    case _: Var => 1
    case _: Index => 1
    case _: Length => 1
    case _: StrSplit => 1
    case _: StrSubStr => 1
  }

  /** Create all permutations of subexpression costs. */
  def costCombinations(): List[List[Int]] = {
    val sum = 0
    val target = currLevel
    val A = ListBuffer.range(1, currLevel)
    val local = ListBuffer[Int]()
    val foundCombos = mutable.Set[List[Int]]()
    costCombinationsUtil(A, sum, target, local, foundCombos)
  }

  def costCombinationsUtil(A: ListBuffer[Int],
                           sum: Int,
                           target: Int,
                           local: ListBuffer[Int],
                           foundCombos: mutable.Set[List[Int]]): List[List[Int]] = {
    for(i <- A.indices) {
      breakable {
        // Check if sum equals target.
        if (sum == target) foundCombos += local.toList
        // Check if the sum exceeds target.
        if (sum + A(i) > target) break
        // Check if the values are the same.
        if (i > 1 && A(i) == A(i - 1)) break
        // Include element in the combination.
        local += A(i)
        // Recursive call
        costCombinationsUtil(A, sum + A(i), target, local, foundCombos)
        // Remove element from the combination.
        local.remove(local.length - 1)
      }
    }
    foundCombos.toList
  }

  def isDiscovered(prog: Expr): Boolean = {
    if (!valueSpace.contains(currLevel)) {
      valueSpace(currLevel) = mutable.ArrayBuffer[Expr](prog)
    }
    valueSpace(currLevel).contains(prog)
  }

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

  def enumerate(): Unit = {
    while (currLevel <= LIM) {
      val candidates = newPrograms()
      while (candidates.hasNext) {
        val prog = candidates.next()
        // Evaluate program.
        val e: List[Value] = eval(ctx,prog)
        if (e == res) {
          // Add program to solution bank.
        } else if (isDiscovered(prog)) {
          // Continue.
        } else if (isPartiallyCorrect(e)) {
          // Update rule's probability.
        }
        currLevelDiscovered += e
      }
      currLevel += 1
    }
  }

  def newPrograms(): Iterator[Expr] = {
    val newProgs = ListBuffer[Expr]()
    for (op <- vocab.operations) {
      // Calculate cost of op.
      val opName = op._1
      val opCost = 0
      val opArity = aryOf(opName)
      val costCombos = costCombinations()
      if (opCost == currLevel && opArity == 0) {
        // Return leaf node.
      } else if (opCost < currLevel && opArity > 0) {
        // Create all permutations of subexpressions.
        val costComboCandidates = costCombos.filter(_.length == opArity)
        for (cost <- costComboCandidates) {
          val childrenLevel = currLevel - 1
          val childrenTypes = childTypes(opName)
          val costZipArity = childrenTypes.map(types => cost zip types)
          val childrenCandidates = costZipArity.map(t => t.map(i => valueSpace(childrenLevel)
            .filter(expr => costMap(expr) == i._1 && typecheck(expr) == i._2)))
            .filter(lst => lst.forall(l => l.nonEmpty))
            .head.map(_.toList)
          val children = cartesianProduct(childrenCandidates)
          for (params <- children) {
            println("Params: " + params)
            newProgs += init(opName, params)
          }
        }
      }
    }
    println(newProgs)
    newProgs.iterator
  }
}
