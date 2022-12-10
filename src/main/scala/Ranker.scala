package org.bottomup.arithmetic

import org.bottomup.arithmetic.Arithmetic.{Env, Expr, Value, eval, mkCodeMultipleCtx}

import scala.collection.mutable.ArrayBuffer

class Ranker(ctx: List[Env[Value]], res: List[Value]) {
//  val rankedList = new ArrayBuffer[Expr]
//
//  def insertHere(ns:Seq[Expr], elem:Expr):Int = {
//    val idx = ns.indexWhere(e => {
//      computeDistance
//      eval(ctx,elem)
//    } eval(ctx,elem))
//    if (idx < 0) ns.length
//    else idx
//  }
//
//  /** Calculate the rank of a candidate program. */
//  def findRank(candidate: Expr): Unit = {
//    /** Calculate the index of the incoming index. */
//    val ind = insertHere(rankedList.toList, candidate)
//    /** Insert the candidate into the ranked list. */
//    rankedList.insert(ind, candidate)
//    /** Calculate the rank  */
//    val candidateRank = rankedList.size - rankedList.indexOf(candidate)
//    println("Rank of " + candidate + " is " + candidateRank)
//  }
//
//  def main(args: Array[String]): Unit = {
//    // array named arr// array named arr
//    val arr = ArrayBuffer(88, 14, 69, 30, 29, 89)
//    // length of arr
//    val len = arr.length
//    for (el <- arr) {
//      findRank(el)
//    }
//  }
}