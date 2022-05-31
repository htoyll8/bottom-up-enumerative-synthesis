package org.bottomup.arithmetic
import Arithmetic.{ErrTy, Expr, Index, IntTy, Bool, Minus, Num, NumArr, NumV, Plus, Str, StrSplit, StrToList, StringArr, StringArrV, StringTy, StringV, Type, Value, Var, aryOf, canMake, eval, init, mkCode, postprocess, tyOf, verify}

import org.bottomup.arithmetic.JsonSerializer.ExprFormat
import org.scalatest.funsuite.AnyFunSuite
import play.api.libs.json.{Format, JsNumber, JsObject, JsResult, JsValue, Json}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.util.control.Breaks.{break, breakable}


object JsonSerializer {

  implicit object ExprFormat extends Format[Expr] {
    override def writes(o: Expr): JsObject = o match {
      case n: Num => Json.obj(
        "op" -> "NUM",
        "n" -> n.n.asInstanceOf[Int]
      )
      case s: Str => Json.obj(
        "op" -> "STR",
        "s" -> s.s
      )
      case b: Bool => Json.obj(
        "op" -> "BOOL",
        "n" -> b.n
      )
      case v: Var => Json.obj(
        "op" -> "VAR",
        "x" -> v.x
      )
      case p: Plus => Json.obj(
        "op" -> "PLUS",
        "children" -> p.children
      )
      case m: Minus => Json.obj(
        "op" -> "MINUS",
        "children" -> m.children
      )
    }

    override def reads(json: JsValue): JsResult[Expr] = ???
  }
}

class EnumTests extends AnyFunSuite {
  test("Combinations") {
    val addition = Plus(List(Num(2), Minus(List(Num(1), Num(5)))))
    val res = ExprFormat.writes(addition)
    println(res)
  }











  test("Enumerate arithmetic 5") {
    val leafNodes = List(
      Var("x"),
      Var("y"),
      Str("Tyler"),
      Str("Holloway")
    )
    val grammar = List(
      "LENGTH",
      "ISALPHA",
      "LOWER",
      "UPPER",
      "INDEX",
      "PLUS",
      "ARRAPPEND",
      "SPLIT",
      "STARTSWITH",
      "IFTHENELSE",
      "STRREPLACE",
      "ENDSWITH",
      "CONCAT",
      // "MINUS",
      "TIMES",
    )
    val ctx = Map(
      Var("x").x -> NumV(3),
      Var("y").x -> NumV(2),
    )

    val typeCtx = Map(
      Var("x").x -> IntTy,
      Var("y").x -> IntTy,
    )

    def typecheck(ast: Expr):Type =
      tyOf(Map.empty,ast);

    def evaluate(ast: Expr):Value =
      eval(ctx, ast)

    /* Enumeration */
    val prevLevelDiscovered = ListBuffer[Expr]()
    val currentLevelDiscovered = mutable.Set[Expr]()

    def enumerateLeafNodes(): Unit = {
      grammar.foreach(exprStr => {
        val childrenCandidates = (leafNodes ++ leafNodes).combinations(aryOf(exprStr))
        while (childrenCandidates.hasNext) {
          val params = childrenCandidates.next()
          val ast = init(exprStr, params)
          if (tyOf(typeCtx, ast) != ErrTy && verify(ctx,typeCtx,ast)) prevLevelDiscovered += ast
        }
      })
    }

    def enumerateNodes(): Unit = {
      val plist = prevLevelDiscovered
      var counter = 0
      for( i <- 1 to 4){
        breakable {
          grammar.foreach(exprStr => {
            val childrenCandidates = (plist ++ plist).combinations(aryOf(exprStr))
            while (childrenCandidates.hasNext) {
              val params = childrenCandidates.next()
              val ast = init(exprStr, params.toList)
              if (tyOf(typeCtx,ast) != ErrTy && verify(ctx,typeCtx,ast)) {
                currentLevelDiscovered += ast
                counter += 1
              }
              if (counter == 200000) {
                println("Breaking...")
                break
              }
            }
          })
        }
        println("Round: " + i)
        prevLevelDiscovered ++= currentLevelDiscovered
        currentLevelDiscovered.clear()
      }
    }



    enumerateLeafNodes()
    enumerateNodes()
    val res = prevLevelDiscovered.map(ast => {
      if (evaluate(ast) == NumV(5)) mkCode(ctx,ast)
    }).toSet


    res.foreach(r => {
      println(r)
      println
    })
  }

  test("Enumerate arithmetic 5 no plus same") {
    val leafNodes = List(
//      Var("x"),
      Num(3),
      Num(1),
      // Var("y"),
      Str(""),
      Var("fname"),
      Var("lname"),
      Str("."),
      //Str("@"),
      //Str("aol")
    )

    //
    // caching
    // memoization
    // symmetry

    val grammar = List(
      "CONCAT",
      "ARRJOIN",
      "INDEX",
      "LENGTH",
      "LOWER",
      "PLUS",
      "SPLIT",
      "STRTOLIST",
    )

    val ctx = Map(
      Var("x").x -> NumV(3),
      Var("y").x -> NumV(2),
      Var("fname").x -> StringV("Mug"),
      Var("lname").x -> StringV("Pug"),
    )

    val typeCtx = Map(
      Var("x").x -> IntTy,
      Var("y").x -> IntTy,
      Var("fname").x -> StringTy,
      Var("lname").x -> StringTy,
    )

    /* Enumeration */
    val valueSpace = mutable.Set[Expr]()
    val prevLevelDiscovered = ListBuffer[Expr]() ++ leafNodes
    val currentLevelDiscovered = mutable.Set[Expr]()

    val mem = mutable.Map[List[Type], ListBuffer[List[Expr]]]()

    def typecheck(ast: Expr):Type =
      tyOf(typeCtx,ast)

    def evaluate(ast: Expr):Value =
      eval(ctx, ast)

    def getIndices(ls: List[Expr]) = ls.map(l => ls.indexOf(l))

    def getExprs(ls: List[Expr], indices: List[Int]) = indices.map(i => ls(i))

    def insert[T](elem: T, list: List[T]) =
      (0 to list.size).map{
        pos => list.take(pos) ++ (elem :: list.drop(pos))
      }

    def perm[T](elements: List[T]) =
      elements.foldLeft(List(List[T]())){
        (results, element) => results.flatMap{insert(element,_)}
      }

    def combinationList(ls: List[Expr], arity: Int) = {
      val indices = getIndices(ls)
      val candidates = indices.combinations(arity).flatMap(c => perm(c))
      candidates
//      candidates.foreach(println)
//      ls.combinations(arity).flatMap(c => {
//        println("C:" + c)
//        c.permutations
//      })
    }

    def takeSample(a:Array[Expr],n:Int,seed:Long) = {
      val rnd = new Random(seed)
      Array.fill(n)(a(rnd.nextInt(a.size)))
    }


    def enumerateLeafNodes(): Unit = {
      grammar.foreach(exprStr => {
        val childrenCandidates = combinationList(leafNodes, aryOf(exprStr)).toList
        val children = childrenCandidates.filter(c => canMake(exprStr, c.map(i => typecheck(leafNodes(i))))).toIterator
        while (children.hasNext) {
          val params = getExprs(leafNodes, children.next)
          val ast = init(exprStr, params)
          if (tyOf(typeCtx, ast) != ErrTy && verify(ctx,typeCtx,ast)) {
            prevLevelDiscovered += ast
          }
        }
      })
    }

    def enumerateNodes(): Unit = {
      var counter = 0
      for( i <- 1 to 6){
        breakable {
          println("Starting loop: " + i)
          grammar.foreach(exprStr => {
            val sample = takeSample(prevLevelDiscovered.toArray, 500, System.currentTimeMillis)
            //val sample = prevLevelDiscovered
            val childrenCandidates = combinationList(sample.toList, aryOf(exprStr)).toList
            val children = childrenCandidates.filter(c => canMake(exprStr, c.map(i => typecheck(sample(i))))).toIterator
            while (children.hasNext) {
              val params = getExprs(sample.toList, children.next)
              val ast = init(exprStr, params)
              if (tyOf(typeCtx,ast) != ErrTy && verify(ctx,typeCtx,ast)) {
                currentLevelDiscovered += ast
                counter += 1
              }

              if (counter == 5000) {
                println("Breaking...")
                break
              }
            }
          })
        }

        valueSpace ++= prevLevelDiscovered
        if (i > 2) prevLevelDiscovered.clear()
        prevLevelDiscovered ++= currentLevelDiscovered
        currentLevelDiscovered.clear()
        currentLevelDiscovered ++= leafNodes
      }
    }

//    def enumerateNodes(): Unit = {
//      var counter = 0
//      for( i <- 1 to 2){
//        breakable {
//          println("Starting loop: " + i)
//          grammar.foreach(exprStr => {
//            println("Length: " + prevLevelDiscovered.length)
//
//            val childrenCandidates = combinationList(prevLevelDiscovered.toList, aryOf(exprStr)).toList
//            // combinationList((0 until aryOf(exprStr)).map(_ => prevLevelDiscovered.toList).toList)
//            val children = childrenCandidates.filter(c => canMake(exprStr, c.map(typecheck))).toIterator
//
//            while (children.hasNext) {
//              val params = children.next()
//              val ast = init(exprStr, params)
//              if (exprStr == "ARRJOIN") println(params)
//              if (tyOf(typeCtx,ast) != ErrTy && verify(ctx,typeCtx,ast)) {
//                if (exprStr == "ARRJOIN") println("Adding: " + ast)
//                currentLevelDiscovered += ast
//                counter += 1
//              } else {
//                if (exprStr == "ARRJOIN") println("Not Adding: " + ast)
//                //println("Type: " + tyOf(typeCtx,ast))
//                //println("Type: " + verify(ctx,typeCtx,ast))
//              }
//
//              if (counter == 5000) {
//                println("Breaking...")
//                break
//              }
//            }
//          })
//        }
//        valueSpace ++= prevLevelDiscovered
//        if (i > 2) prevLevelDiscovered.clear()
//        prevLevelDiscovered ++= currentLevelDiscovered
//        currentLevelDiscovered.clear()
//        currentLevelDiscovered ++= leafNodes
//      }
//    }

    enumerateLeafNodes()
    enumerateNodes()
    //prevLevelDiscovered.foreach(ast => println(mkCode(ctx, ast)))
    valueSpace.foreach(ast => if(typecheck(ast) == StringTy) println(mkCode(ctx, ast), evaluate(ast)))

    val res = valueSpace.map(ast => {
      if (evaluate(ast) == StringV("mug.pug")) mkCode(ctx,ast)
    }).toSet

    res.foreach(r => {
      println(r)
      println
    })
  }






  test("Enumerate arithmetic phone-1") {
    val leafNodes = List(
      Var("num"),
      Var("sep"),
    )

    val grammar = List(
      "CONCAT",
      "ARRJOIN",
      "STRREPLACE",
      "INDEX",
      "LENGTH",
      "LOWER",
      "PLUS",
      "SPLIT",
      "STRTOLIST",
    )

    val ctx = Map(
      Var("num").x -> StringV("203.243.8890"),
      Var("sep").x -> StringV("-"),
    )

    val typeCtx = Map(
      Var("num").x -> StringTy,
      Var("sep").x -> StringTy,
    )

    /* Enumeration */
    val valueSpace = mutable.Set[Expr]()
    val prevLevelDiscovered = ListBuffer[Expr]() ++ leafNodes
    val currentLevelDiscovered = mutable.Set[Expr]()

    val mem = mutable.Map[List[Type], ListBuffer[List[Expr]]]()

    def typecheck(ast: Expr):Type =
      tyOf(typeCtx,ast)

    def evaluate(ast: Expr):Value =
      eval(ctx, ast)

    def getIndices(ls: List[Expr]) = ls.map(l => ls.indexOf(l))

    def getExprs(ls: List[Expr], indices: List[Int]) = indices.map(i => ls(i))

    def insert[T](elem: T, list: List[T]) =
      (0 to list.size).map{
        pos => list.take(pos) ++ (elem :: list.drop(pos))
      }

    def perm[T](elements: List[T]) =
      elements.foldLeft(List(List[T]())){
        (results, element) => results.flatMap{insert(element,_)}
      }

    def combinationList(ls: List[Expr], arity: Int) = {
      val indices = getIndices(ls)
      indices.combinations(arity).flatMap(c => perm(c))
    }

    def takeSample(a:Array[Expr],n:Int,seed:Long) = {
      val rnd = new Random(seed)
      Array.fill(n)(a(rnd.nextInt(a.size)))
    }

    def enumerateLeafNodes(): Unit = {
      grammar.foreach(exprStr => {
        val childrenCandidates = combinationList(leafNodes, aryOf(exprStr)).toList
        val children = childrenCandidates.filter(c => canMake(exprStr, c.map(i => typecheck(leafNodes(i))))).toIterator
        while (children.hasNext) {
          val params = getExprs(leafNodes, children.next)
          val ast = init(exprStr, params)
          if (tyOf(typeCtx, ast) != ErrTy && verify(ctx,typeCtx,ast)) {
            prevLevelDiscovered += ast
          }
        }
      })
    }

    def enumerateNodes(): Unit = {
      var counter = 0
      for( i <- 1 to 3){
        breakable {
          println("Starting loop: " + i)
          grammar.foreach(exprStr => {
            val sample = takeSample(prevLevelDiscovered.toArray, 500, System.currentTimeMillis)
            //val sample = prevLevelDiscovered
            val childrenCandidates = combinationList(sample.toList, aryOf(exprStr)).toList
            val children = childrenCandidates.filter(c => canMake(exprStr, c.map(i => typecheck(sample(i))))).toIterator
            while (children.hasNext) {
              val params = getExprs(sample.toList, children.next)
              val ast = init(exprStr, params)
              if (tyOf(typeCtx,ast) != ErrTy && verify(ctx,typeCtx,ast)) {
                currentLevelDiscovered += ast
                counter += 1
              }

              if (counter == 5000) {
                println("Breaking...")
                break
              }
            }
          })
        }

        valueSpace ++= prevLevelDiscovered
        if (i > 2) prevLevelDiscovered.clear()
        prevLevelDiscovered ++= currentLevelDiscovered
        currentLevelDiscovered.clear()
        currentLevelDiscovered ++= leafNodes
      }
    }

    enumerateLeafNodes()
    enumerateNodes()
    //prevLevelDiscovered.foreach(ast => println(mkCode(ctx, ast)))
    valueSpace.foreach(ast => /*if(typecheck(ast) == StringTy)*/ println(mkCode(ctx, ast), evaluate(ast)))
    val res = valueSpace.map(ast => {
      if (evaluate(ast) == NumV(3)) mkCode(ctx,ast)
    }).toSet
    res.foreach(r => {
      println(r)
      println
    })
  }






  // Median list of numbers
  // Sorted not sort,
  // Reverse..

  test("Enumerate arithmetic four") {
    val leafNodes = List(
      Var("x"),
      Num(0),
      Num(1),
      Num(2),
      Num(3),
      Num(4),
      // Str("Pour"),
      Str(" "),
    )

    val grammar = List(
      "SPLIT",
      "INDEX",
      "LENGTH",
    )
    val ctx = Map(
      Var("x").x -> StringV("Tour Lour Pour Kour"),
      Var("y").x -> NumV(2),
    )

    val typeCtx = Map(
      Var("x").x -> StringTy,
      Var("y").x -> IntTy,
    )

    def typecheck(ast: Expr):Type =
      tyOf(typeCtx,ast);

    def evaluate(ast: Expr):Value =
      eval(ctx, ast)

    def combinationList[T](ls: List[T], arity: Int) = {
      ls.combinations(arity).flatMap(_.permutations)
    }

    /* Enumeration */
    val valueSpace = mutable.Set[Expr]()
    val prevLevelDiscovered = ListBuffer[Expr]() ++ leafNodes
    val currentLevelDiscovered = mutable.Set[Expr]()

    def enumerateLeafNodes(): Unit = {
      grammar.foreach(exprStr => {
        val childrenCandidates = combinationList(leafNodes, aryOf(exprStr)).toList
        val children = childrenCandidates.filter(c => canMake(exprStr, c.map(typecheck))).toIterator
        while (children.hasNext) {
          val params = children.next()
          val ast = init(exprStr, params)
          if (tyOf(typeCtx, ast) != ErrTy && verify(ctx,typeCtx,ast)) {
            prevLevelDiscovered += ast
          }
        }
      })
    }


    def enumerateNodes(): Unit = {
      var counter = 0
      for( i <- 1 to 3){
        breakable {
          println("Starting loop: " + i)
          grammar.foreach(exprStr => {
            println("Length: " + prevLevelDiscovered.length)
            val childrenCandidates = combinationList(prevLevelDiscovered.toList, aryOf(exprStr)).toList
              // combinationList((0 until aryOf(exprStr)).map(_ => prevLevelDiscovered.toList).toList)
            val children = childrenCandidates.filter(c => canMake(exprStr, c.map(typecheck))).toIterator

            while (children.hasNext) {
              val params = children.next()
              val ast = init(exprStr, params)
              //if (exprStr == "INDEX") println(ast, tyOf(typeCtx,ast), verify(ctx,ast))
              if (tyOf(typeCtx,ast) != ErrTy && verify(ctx,typeCtx,ast)) {
                currentLevelDiscovered += ast
                counter += 1
              } else {
                //if (exprStr == "LENGTH") println("Not Adding: " + ast)
                //println("Type: " + tyOf(typeCtx,ast))
                //println("Type: " + verify(ctx,typeCtx,ast))
              }

              if (counter == 5000) {
                println("Breaking...")
                break
              }
            }
          })
        }
        valueSpace ++= prevLevelDiscovered
        if (i > 2) prevLevelDiscovered.clear()
        prevLevelDiscovered ++= currentLevelDiscovered
        currentLevelDiscovered.clear()
      }
    }





    enumerateLeafNodes()
    enumerateNodes()
    valueSpace.foreach(println)

    val res = valueSpace.map(ast => {
      if (evaluate(ast) == NumV(4)) mkCode(ctx,ast)
    }).toSet

    res.foreach(r => {
      println(r)
      println
    })
  }













  test("Enumerate arithmetic 5 no plus") {
    val leafNodes = List(
      Var("x"),
      Var("y"),
      Var("fname"),
      Var("lname")
    )
    val grammar = List(
      "LENGTH",
      "ISALPHA",
      //"LOWER",
      //"UPPER",
      "INDEX",
      //"PLUS",
      "ARRAPPEND",
      "ARRJOIN",
      "SPLIT",
      "STRTOLIST",
      "STARTSWITH",
//      "IFTHENELSE",
      "STRREPLACE",
      "ENDSWITH",
      "CONCAT",
      // "MINUS",
      "TIMES",
    )
    val ctx = Map(
      Var("x").x -> NumV(3),
      Var("y").x -> NumV(2),
      Var("fname").x -> StringV("Tyler"),
      Var("lname").x -> StringV("Holloway"),
    )

    val typeCtx = Map(
      Var("x").x -> IntTy,
      Var("y").x -> IntTy,
      Var("fname").x -> StringTy,
      Var("lname").x -> StringTy,
    )

    def typecheck(ast: Expr):Type =
      tyOf(Map.empty,ast);

    def evaluate(ast: Expr):Value =
      eval(ctx, ast)

    /* Enumeration */
    val prevLevelDiscovered = ListBuffer[Expr]()
    val currentLevelDiscovered = mutable.Set[Expr]()

    def enumerateLeafNodes(): Unit = {
      grammar.foreach(exprStr => {
        val childrenCandidates = (leafNodes ++ leafNodes).combinations(aryOf(exprStr))
        while (childrenCandidates.hasNext) {
          val params = childrenCandidates.next()
          val ast = init(exprStr, params)
          if (tyOf(typeCtx, ast) != ErrTy && verify(ctx,typeCtx,ast)) prevLevelDiscovered += ast
        }
      })
    }

    def enumerateNodes(): Unit = {
      val plist = prevLevelDiscovered
      var counter = 0
      for( i <- 1 to 8){
        breakable {
          grammar.foreach(exprStr => {
            val childrenCandidates = (plist ++ plist).combinations(aryOf(exprStr))
            while (childrenCandidates.hasNext) {
              val params = childrenCandidates.next()
              val ast = init(exprStr, params.toList)
              if (tyOf(typeCtx,ast) != ErrTy && verify(ctx,typeCtx,ast)) {
                currentLevelDiscovered += ast
                counter += 1
              }
              if (counter == 200000) {
                println("Breaking...")
                break
              }
            }
          })
        }
        println("Round: " + i)
        prevLevelDiscovered ++= currentLevelDiscovered
        currentLevelDiscovered.clear()
      }
    }

    enumerateLeafNodes()
    enumerateNodes()
    val res = prevLevelDiscovered.map(ast => {
      if (evaluate(ast) == NumV(5)) mkCode(ctx,ast)
    }).toSet


    res.foreach(r => {
      println(r)
      println
    })
  }







  test("Enumerate arithmetic") {
    val leafNodes = List(
      Var("x"),
      Var("y"),
      Str("Tyler"),
      Str("Holloway")
    )
    val grammar = List(
      "LENGTH",
      "ISALPHA",
      "PLUS",
      //"LOWER",
      //"UPPER",
      "INDEX",
      "ARRAPPEND",
      "SPLIT",
      "STARTSWITH",
      "IFTHENELSE",
      "STRREPLACE",
      "ENDSWITH",
      "CONCAT",
      "MINUS",
      "TIMES",
    )
    val ctx = Map(
      Var("x").x -> NumV(3),
      Var("y").x -> NumV(2),
    )

    val typeCtx = Map(
      Var("x").x -> IntTy,
      Var("y").x -> IntTy,
    )

    def typecheck(ast: Expr):Type =
      tyOf(Map.empty,ast);

    def evaluate(ast: Expr):Value =
      eval(ctx, ast)

    /* Enumeration */
    val prevLevelDiscovered = ListBuffer[Expr]()
    val currentLevelDiscovered = mutable.Set[Expr]()

    def enumerateLeafNodes(): Unit = {
      grammar.foreach(exprStr => {
        val childrenCandidates = (leafNodes ++ leafNodes).combinations(aryOf(exprStr))
        while (childrenCandidates.hasNext) {
          val params = childrenCandidates.next()
          val ast = init(exprStr, params)
          if (tyOf(typeCtx, ast) != ErrTy && verify(ctx,typeCtx,ast)) prevLevelDiscovered += ast
        }
      })
    }

    def enumerateNodes(): Unit = {
      val plist = prevLevelDiscovered
      var counter = 0
      for( i <- 1 to 4){
        breakable {
          grammar.foreach(exprStr => {
            val childrenCandidates = (plist ++ plist).combinations(aryOf(exprStr))
            while (childrenCandidates.hasNext) {
              val params = childrenCandidates.next()
              val ast = init(exprStr, params.toList)
              if (tyOf(typeCtx,ast) != ErrTy && verify(ctx,typeCtx,ast)) {
                currentLevelDiscovered += ast
                counter += 1
              }
              if (counter == 200000) {
                println("Breaking...")
                break
              }
            }
          })
        }
        println("Round: " + i)
        prevLevelDiscovered ++= currentLevelDiscovered
        currentLevelDiscovered.clear()
      }
    }

    enumerateLeafNodes()
    enumerateNodes()
    val res = prevLevelDiscovered.map(ast => {
      if (evaluate(ast) == StringV("tylerh")) mkCode(ctx,ast)
    }).toSet


    res.foreach(r => {
      println(r)
      println
    })
  }


  test("Enumerate arithmetic Tyler Holloway -> tylerh@gmail.com") {
    val leafNodes = List(
      Var("x"),
      Var("y"),
      Var("fname"),
      Var("lname"),
      Str("@gmail.com")
      //      Str(" "),
    )
    val grammar = List(
      //      "LENGTH",
      "INDEX",
      "LOWER",
      "UPPER",
      "CONCAT",
      //      "IFTHENELSE",
      "STARTSWITH",
      //      "SPLIT",
      //      "ARRINDEX",
      //      "ARRJOIN",
      "ISALPHA",
      "PLUS",
      //      "ARRAPPEND",
      "STRREPLACE",
      //      "ENDSWITH",
      "MINUS",
      //      "TIMES",
    )
    val ctx = Map(
      Var("x").x -> NumV(0),
      Var("y").x -> NumV(2),
      Var("fname").x -> StringV("Tyler"),
      Var("lname").x -> StringV("Holloway"),
    )

    val typeCtx = Map(
      Var("x").x -> IntTy,
      Var("y").x -> IntTy,
      Var("fname").x -> StringTy,
      Var("lname").x -> StringTy,
    )

    def typecheck(ast: Expr):Type =
      tyOf(Map.empty,ast);

    def evaluate(ast: Expr):Value =
      eval(ctx, ast)

    /* Enumeration */
    val prevLevelDiscovered = ListBuffer[Expr]()
    val currentLevelDiscovered = mutable.Set[Expr]()

    def enumerateLeafNodes(): Unit = {
      grammar.foreach(exprStr => {
        val childrenCandidates = (leafNodes ++ leafNodes).combinations(aryOf(exprStr))
        while (childrenCandidates.hasNext) {
          val params = childrenCandidates.next()
          val ast = init(exprStr, params)
          if (tyOf(typeCtx, ast) != ErrTy && verify(ctx,typeCtx,ast)) prevLevelDiscovered += ast
        }
      })
    }

    def enumerateNodes(): Unit = {
      val plist = prevLevelDiscovered
      var counter = 0
      for( i <- 1 to 8){
        breakable {
          grammar.foreach(exprStr => {
            val childrenCandidates = (plist ++ plist).combinations(aryOf(exprStr))
            while (childrenCandidates.hasNext) {
              val params = childrenCandidates.next()
              val ast = init(exprStr, params.toList)
              if (tyOf(typeCtx,ast) != ErrTy && verify(ctx,typeCtx,ast)) {
                currentLevelDiscovered += ast
              }
              counter += 1
              if (counter == 100000) {
                println("Breaking...")
                counter = 0
                break
              }
            }
          })
        }
        println("Round: " + i)
        prevLevelDiscovered ++= currentLevelDiscovered
        currentLevelDiscovered.clear()
      }
    }

    enumerateLeafNodes()
    enumerateNodes()
    val res = prevLevelDiscovered.map(ast => {
      if (evaluate(ast) == StringV("tylerh")) mkCode(ctx,ast)
    }).toSet

    val res2 = prevLevelDiscovered.map(ast => {
      if (evaluate(ast) == StringV("@gmail.com")) mkCode(ctx,ast)
    }).toSet

    // prevLevelDiscovered.foreach(ast => println(evaluate(ast)))

    res.foreach(r => {
      println(r)
      println
    })

    println("============")

    res2.foreach(r => {
      println(r)
      println
    })
  }
}
