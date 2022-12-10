package org.bottomup.arithmetic

import Arithmetic.{BoolV, CharArrV, CharV, Env, ErrTy, Expr, Index, Length, Num, NumArrV, NumV, Str, StrSplit, StrSubStr, StringArrV, StringV, Type, Value, Var, Variable, aryOf, astSize, childTypes, childrenAstSize, eval, evalUtil, init, mkCode, mkCodeMultipleCtx, tyOf, verify, verifyMultipleCtx}

import org.json4s.native.JsonParser.StringVal

import scala.Double.PositiveInfinity
import scala.collection.mutable
import scala.collection.mutable._
import scala.language.postfixOps
import scala.util.control.Breaks.{break, breakable}

class Enumeration(vocab: Vocab,
                  typeCtx: Env[Type],
                  ctx: List[Env[Value]],
                  res: List[Value],
                  LIM: Int) {

  val LOG = false

  /** The current level (e.g., the round). */
  var currLevel: Int = 1

  /** The score of the highest-ranked candidate program. */
  var bestScore: Double = PositiveInfinity

  /** The candidate program with the best score. */
  var bestCandidate: Option[Expr] = None

  /** Top-k candidates. */
  var topKCandidates: ArrayBuffer[Expr] = ArrayBuffer[Expr]()

  /** Current k. */
  var k = 0

  /** Top-k number */
  val top_k_ceiling = 10

  /** Sample size for k-secretary problem. */
  var sampleSize = 20

  /** Ranked list of candidate programs. */
  val rankedList = new ArrayBuffer[Expr]

  /** The current candidate (e.g., the ith candidate). */
  var candidateCount: Int = 0

  /** The current candidate of the current level (e.g., the ith candidate). */
  var curLevelCandidateCount: Int = 0

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

  /** Initialize level.  */
  def initLevel(): Unit =  valueSpace(currLevel) = ArrayBuffer[Expr]() ++ vocab.leafNodes

  /** Add leaf nodes to the initial level of program bank.  */
  def initProgramBank(): Unit =  {
    valueSpace(currLevel) = ArrayBuffer[Expr]() ++ vocab.leafNodes
    currLevel += 1
  }

  /** Add element to the value sapce.  */
  def updateValueSpace(prog: Expr): Unit =  {
    valueSpace(currLevel) += prog
  }

  /** Cartesian product.  */
  def cartesianProduct(x: List[List[Expr]]): List[List[Expr]] = x match {
    case Nil    => List(Nil)
    case h :: _ => h.flatMap(i => cartesianProduct(x.tail).map(i :: _))
  }

  /** Retrieve all programs that satisfy specification.  */
  def computeResults(): List[Expr] = {
    valueSpace.flatMap(_._2).filter(e => eval(ctx,e) == res).toList
  }

  def sortValueSpace(): List[Expr] = {
    val candidates: List[Expr] = valueSpace.flatMap(x => x._2).toList.distinct
    candidates.sortBy(x => findRank(x)).slice(0,top_k_ceiling)
  }

  /** Create all permutations of subexpression costs. */
  def costCombinations(): List[List[Int]] = {
    val sum = 0
    val target = currLevel
    val A = ListBuffer.range(1, currLevel)
    val local = ListBuffer[Int]()
    val foundCombos = mutable.Set[List[Int]](List(currLevel))
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

  /** Calculate Levenshtein Distance between two strings. */
  def distance(s1: String, s2: String): Int = {
    val dist = Array.tabulate(s2.length + 1, s1.length + 1) { (j, i) => if (j == 0) i else if (i == 0) j else 0 }

    @inline
    def minimum(i: Int*): Int = i.min

    for {j <- dist.indices.tail
         i <- dist(0).indices.tail} dist(j)(i) =
      if (s2(j - 1) == s1(i - 1)) dist(j - 1)(i - 1)
      else minimum(dist(j - 1)(i) + 1, dist(j)(i - 1) + 1, dist(j - 1)(i - 1) + 1)

    dist(s2.length)(s1.length)
  }

  /** Create a list of result values. */
  def getResValues(resVals: List[Value]): List[String] = {
    resVals.map {
      case v1: StringV =>
        v1.s
      case v1: NumV =>
        v1.n.toString
      case v1: CharV =>
        v1.n.toString
      case v1: BoolV =>
        v1.n.toString
      case v1: NumArrV =>
        v1.c.toString
      case v1: CharArrV =>
        v1.c.toString
      case v1: StringArrV =>
        v1.c.toString
    }
  }

  /** Calculate Levenshtein Distance between the result of the discovered program and expected results. */
  def computeDistance(e: List[Value]): Double = {
    // String representation of the discovered.
    val discoveredRes: List[String] = getResValues(e)
    // String representation of outputs.
    val expectedRes: List[String] = getResValues(res)
    // Calculate and return Levenshtein Distance between the program and result.
    val dist = distance(discoveredRes.head, expectedRes.head)
    // println(discoveredRes.head, expectedRes.head, dist)
    dist
  }

  /** Calculate the index a new element should be inserted in the ranked list candidate program. */
  def insertHere(ns:List[Expr], elem:Expr):Int = {
    val idx = ns.indexWhere(e => computeDistance(eval(ctx,e)) > computeDistance(eval(ctx,elem)))
    if (idx < 0) ns.length
    else idx
  }

  /** Calculate the rank of a candidate program. */
  def findRank(candidate: Expr): Int = {
    if (!rankedList.contains(candidate)) {
      /** Calculate the index of the incoming index. */
      val ind = insertHere(rankedList.toList, candidate)
      /** Insert the candidate into the ranked list. */
      rankedList.insert(ind, candidate)
      /** Calculate the rank  */
      val candidateRank = rankedList.size - rankedList.indexOf(candidate)
      // println("Rank of " + candidate + " is " + candidateRank)
      return candidateRank
    }
    return rankedList.indexOf(candidate)
  }

  /** Under construction... */
  def calculateProbP(i: Int, j: Int, K: Int): Int = {
    var sum: Int = 0
    for (_ <- 0 until i.min(K)) {
      sum += calculateProbQ(i, j, K)
    }
    i/1 * sum
  }

  def calculateProbQ(i: Int, j: Int, K: Int): Int = {
    var sum: Int = 0
    var sum2: Int = 0

    for (_ <- 0 until j+1) sum += calculateProbP(i, j-1, K)
    for (_ <- 0 until j+1) sum2 += calculateProbP(i, j, K)

    sum - sum2
  }

  def enumerate(): Unit = {
    while (currLevel <= LIM) {
      // println("Level: " + currLevel)
      initLevel()
      val candidates = newPrograms()
      while (candidates.hasNext) {
        breakable {
          val prog = candidates.next()
          // Evaluate program.
          val e: List[Value] = eval(ctx,prog)
          // Update the highest score; the minimum of the two values.
          val rank: Int = findRank(prog)
          // Add to value space.
          updateValueSpace(prog)
          // Update the current level list.
          currLevelDiscovered += e
          // Update the overall candidate count.
          candidateCount += 1
          // Update the current level candidate count.
          curLevelCandidateCount += 1

          // Record the best candidate in the sample, s_r.
          if (candidateCount == sampleSize-1) {
            bestScore = computeDistance(e)
            topKCandidates.append(rankedList.head)
            k += 1
          }

          // Select any candidate that's better than s_r.
          if (candidateCount >= sampleSize) {
            if (computeDistance(e) < bestScore && !topKCandidates.contains(prog)) {
              topKCandidates.append(prog)
              k += 1
            }
          }

          if (k == (top_k_ceiling + 1)) {
            println("Ending... ")
//            return
          }
        }
      }
      println("Level " + currLevel + ": " + curLevelCandidateCount)
      currLevel += 1
      curLevelCandidateCount = 0

      if (k == (top_k_ceiling + 1)) {
        println("Ending... ")
//        return
      }
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
          val childrenLevels = (1 until currLevel).flatMap(level => valueSpace(level))
          val childrenTypes = childTypes(opName)
          val costZipArity = childrenTypes.map(types => cost zip types).head
          val childrenCandidates: ListBuffer[List[Expr]] = ListBuffer[List[Expr]]()
          for ((cost, aType) <- costZipArity) {
              childrenCandidates += childrenLevels.filter(expr => astSize(expr) == cost && typecheck(expr) == aType).toList
          }
          // Initialize AST based on valid parameters.
          if (childrenCandidates.forall(_.nonEmpty)) {
            val childrenCombos = cartesianProduct(childrenCandidates.toList)
            val childrenParams = childrenCombos.filter(children => {
              val res = verifyMultipleCtx(opName, children, ctx, typeCtx)
              res
            })
            // Filter for self-loops (e.g., programs that don't change the subexpression value)
            val programsToAdd: ListBuffer[Expr] = ListBuffer[Expr]()
            for(child <- childrenParams) {
              val newAST = init(opName, child)
              if (child.forall(childAst => eval(ctx, childAst) != eval(ctx, newAST))) {
                programsToAdd += newAST
              }
            }
            newProgs ++= programsToAdd
          }
        }
      }
    }
    newProgs.iterator
  }
}
