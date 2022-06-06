package org.bottomup.arithmetic

import Arithmetic.{Env, ErrTy, Expr, NumV, Type, Value, Var, Variable, aryOf, astSize, canMake, eval, evalMultipleCtx, init, mkCode, mkCodeMultipleCtx, tyOf, verify, verifyMultipleCtx}

import org.bottomup.arithmetic.JsonSerializer._
import play.api.libs.json.JsObject

import scala.concurrent.duration.{DurationInt, pairLongToDuration}
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
    val operations = grammar.operations.toList.sortBy(_._2)
    prevLevelDiscovered ++= leafNodes
    def typecheck(ast: Expr):Type = tyOf(typeCtx,ast)
    operations.foreach(expr => {
      val exprStr = expr._1
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
    val leafNodes = grammar.leafNodes
    var operations = grammar.operations.toList.sortBy(_._2)
    val deadline = timeout.seconds.fromNow

    def typecheck(ast: Expr):Type = tyOf(typeCtx,ast)

    def computeRes() = {
      val matches = valueSpace.filter(e => evalMultipleCtx(ctx, e) == res).toList.sortBy(astSize)
      //matches.foreach(m => println(m, astSize(m)))
      matches.foreach(e => println(mkCodeMultipleCtx(ctx, e)))
    }

    while(deadline.hasTimeLeft) {
      var i = 0
      operations = grammar.operations.toList.sortBy(_._2)
      breakable {
        prevLevelDiscovered ++= leafNodes
        val plist = prevLevelDiscovered.toList.sortBy(astSize)

        operations.foreach(expr => {
          var counter = 0
          breakable{
            val exprStr = expr._1
            println("Creating children for: " + exprStr + plist.length)
            val childrenCandidates: List[List[Expr]] = combinationList(plist, aryOf(exprStr)).toList
            val children = childrenCandidates.filter(c => {
              val childrenCandidateTypes = c.map(typecheck)
              canMake(exprStr, childrenCandidateTypes)
            })

            if (exprStr == "INDEX") children.foreach(println)
            println("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++x")
            println("Created Length: " + exprStr + " " + children.length)
            val childrenIterator = children.iterator

            while (childrenIterator.hasNext) {
              val params = childrenIterator.next()
              val ast = init(exprStr, params)
              if (tyOf(typeCtx, ast) != ErrTy && verifyMultipleCtx(ctx,typeCtx,ast)) {
                if (evalMultipleCtx(ctx, ast) == res) grammar.operations(exprStr) += 1
                currentLevelDiscovered += ast
              }
            }
            println("Finished iterating over children")
          }
        })

        // Change level
        valueSpace ++= prevLevelDiscovered
        prevLevelDiscovered ++= currentLevelDiscovered
        currentLevelDiscovered.clear()
        i += 1
      }
    }
    //flushLevel(currentLevelDiscovered)
    valueSpace ++= currentLevelDiscovered
    computeRes()
    //valueSpace.toList.sortBy(astSize).foreach(e => println(mkCodeMultipleCtx(ctx, e)))
   // closeFile()
  }

  def writeJson(exprs: List[Expr]): List[JsObject] = exprs.map(ExprFormat.writes)

  def writeFile(s: String): Unit = bw.write(s+"\n")

  def closeFile(): Unit = bw.close()

  def combinationList(ls: List[Expr], arity: Int): Iterator[List[Expr]] = {
    println("Combining: " + ls.length)
    val perms = ListBuffer[List[Expr]]()
    val combos = ls.combinations(arity)
    var counter = 0
    for (c <- combos if counter > 2000) {
      //c.permutations.toList.foreach(p => perms += p)
      perms += c
      counter += 1
    }
    perms.iterator
  }

  def flushLevel(level: ListBuffer[Expr]): Unit = {
    for (expr <-level) writeFile(ExprFormat.writes(expr).toString)
    level.clear()
  }
}
