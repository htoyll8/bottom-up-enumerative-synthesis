package org.bottomup.arithmetic
import Arithmetic.{Bool, Concat, ErrTy, Expr, Index, IntTy, Minus, Num, NumArr, NumV, Plus, Str, StrIndex, StrSplit, StrToList, StringArr, StringArrTy, StringArrV, StringTy, StringV, Type, Value, Var, aryOf, astSize, eval, init, mkCode, mkCodeMultipleCtx, tyOf, verify}

import org.scalatest.funsuite.AnyFunSuite

import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class EnumTests extends AnyFunSuite {
  test("First four chars") {
    val vocab = Vocab(List(
      Var("str"),
      Var("sep"),
      Num(0),
      Num(1),
      Num(2),
      Num(4)
    ),
      mutable.Map(
        "SPLIT" -> 0,
        "STRSUBSTR" -> 0,
        "LENGTH" -> 0,
        "STRINDEX" -> 0,
        "ARRINDEX" -> 0
      )
    )

    val ctx = List(
      Map(
        Var("str").x -> StringV("EC1A-1BB-AC12"),
        Var("sep").x -> StringV("-")
      ),
      Map(
        Var("str").x -> StringV("DN55-IPT-AB34"),
        Var("sep").x -> StringV("-")
      ),
      Map(
        Var("str").x -> StringV("ASCN-1ZZ-CD89"),
        Var("sep").x -> StringV("-")
      ),
      Map(
        Var("str").x -> StringV("STHL-1ZZ-P065"),
        Var("sep").x -> StringV("-")
      )
    )

    val typeCtx = Map(
      Var("str").x -> StringTy,
      Var("sep").x -> StringTy
    )

    val res = List(
      StringV("EC1A"),
      StringV("DN55"),
      StringV("ASCN"),
      StringV("STHL")
    )

    val enumerator = new Enumeration(vocab, typeCtx, ctx, res, 11)
    enumerator.initProgramBank()
    enumerator.enumerate()

    // Printer set up.
    val writer = new PrintWriter(new File("first-four-char-outputFile.txt"))  //specify the file path
    val results = enumerator.computeResults().map(e => mkCodeMultipleCtx(ctx, e)).toSet
    results.foreach(res => writer.println(res.mkString("\"", "", "\",")))
    writer.close()
  }

  test("Shorten thousand dollars") {
    val vocab = Vocab(List(
      Var("amount"),
      Var("sep"),
      Str("k"),
      Num(0),
      Num(2)
    ),
      mutable.Map(
        "SPLIT" -> 0,
        "STRINDEX" -> 0,
        "ARRINDEX" -> 0,
        "CONCAT" -> 0,
        "STRSUBSTR" -> 0,
        "STRREPLACE" -> 0,
        "LENGTH" -> 0,
      )
    )

    val ctx = List(
      Map(
        Var("amount").x -> StringV("80,000"),
        Var("sep").x -> StringV(",")
      )
    )

    val typeCtx = Map(
      Var("amount").x -> StringTy,
      Var("sep").x -> StringTy
    )

    val res = List(
      StringV("80k")
    )

    val enumerator = new Enumeration(vocab, typeCtx, ctx, res, 11)
    enumerator.initProgramBank()
    enumerator.enumerate()

    // Printer set up.
    val writer = new PrintWriter(new File("shorten-thousand-outputFile.txt"))  //specify the file path
    val results = enumerator.computeResults().map(e => mkCodeMultipleCtx(ctx, e)).toSet
    results.foreach(writer.println)
    writer.close()
  }

  test("Email name") {
    val vocab = Vocab(List(
      Var("row"),
      Num(0),
      Num(1),
      Num(2),
      Str("@")
    ),
      mutable.Map(
        "SPLIT" -> 0,
        "STRSUBSTR" -> 0,
        "CONCAT" -> 0,
        "LOWER" -> 0,
        "LENGTH" -> 0,
        "STRINDEX" -> 0,
        "ARRINDEX" -> 0
      )
    )

    val ctx = List(
      Map(
        Var("row").x -> StringArrV(ListBuffer("Amy","Chang","achang@gmail.com"))
      ),
      Map(
        Var("row").x -> StringArrV(ListBuffer("Bobby","Smith","bsmith@aol.com"))
      ),
      Map(
        Var("row").x -> StringArrV(ListBuffer("Thomas","Payne","tpayne@hotmail.com"))
      )
    )

    val typeCtx = Map(
      Var("row").x -> StringArrTy
    )

    val res = List(
      StringV("achang"),
      StringV("bsmith"),
      StringV("tpayne")
    )

    val enumerator = new Enumeration(vocab, typeCtx, ctx, res, 7)
    enumerator.initProgramBank()
    enumerator.enumerate()

    // Printer set up.
    val writer = new PrintWriter(new File("email-name-outputFile.txt"))  //specify the file path
    val results = enumerator.computeResults().map(e => mkCodeMultipleCtx(ctx, e)).toSet
    results.foreach(res => writer.println(res + ","))
    writer.close()

    // Print top-k programs.
    //    val topCandidates = enumerator.topKCandidates.slice(0,10).map(e => (mkCodeMultipleCtx(ctx, e), eval(ctx, e)))
    //    for (x <- topCandidates) print(x)
    val topCandidates = enumerator.sortValueSpace().map(e => (mkCodeMultipleCtx(ctx, e), eval(ctx, e)))
    for (x <- topCandidates) {
      println(x)
    }
  }

  test("Employee ID") {
    val vocab = Vocab(List(
      Var("row"),
      Var("sep"),
      Num(0),
      Num(1),
      Num(2),
      Num(3),
      Num(-3),
      Num(4)
    ),
      mutable.Map(
        "SPLIT" -> 0,
        "REVERSESTRSUBSTR" -> 0,
        "STRSUBSTR" -> 0,
        "CONCAT" -> 0,
        "TITLE" -> 0,
        "LOWER" -> 0,
        "LENGTH" -> 0,
        "STRINDEX" -> 0,
        "ARRINDEX" -> 0
      )
    )

    val ctx = List(
      Map(
        Var("row").x -> StringArrV(ListBuffer("Jim","Anderson","111223333", "And", "333")),
        Var("sep").x -> StringV(",")
      ),
    )

    val typeCtx = Map(
      Var("row").x -> StringArrTy,
      Var("sep").x -> StringTy
    )

    val res = List(
      StringV("And333"),
    )

    val enumerator = new Enumeration(vocab, typeCtx, ctx, res, 11)
    enumerator.initProgramBank()
    enumerator.enumerate()

    // Printer set up.
    val writer = new PrintWriter(new File("employee-id-value-space.txt"))  //specify the file path
    val results = enumerator.computeResults().map(e => mkCodeMultipleCtx(ctx, e)).toSet
    results.foreach(writer.println)
    writer.close()
  }

  test("Transform email") {
    val vocab = Vocab(List(
      Var("row"),
      Var("sep"),
      Var("domain"),
      Str("example.com"),
      Str("@"),
      Str("."),
      Num(0),
      Num(1),
      Num(2)
    ),
      mutable.Map(
        "SPLIT" -> 0,
        "LENGTH" -> 0,
        "STRINDEX" -> 0,
        "ARRINDEX" -> 0,
        "CONCAT" -> 0,
        "STRSUBSTR" -> 0,
        "STRREPLACE" -> 0,
        "LENGTH" -> 0,
      )
    )

    val ctx = List(
      Map(
        Var("row").x -> StringArrV(ListBuffer("Adam","Abraham","Adam.Abraham@example.com")),
        Var("sep").x -> StringV(","),
        Var("domain").x -> StringV("gmail.com")
      ),
    )

    val typeCtx = Map(
      Var("row").x -> StringArrTy,
      Var("sep").x -> StringTy,
      Var("domain").x -> StringTy
    )

    val res = List(
      StringV("Adam.Abraham@gmail.com"),
    )

    val enumerator = new Enumeration(vocab, typeCtx, ctx, res, 8)
    enumerator.initProgramBank()
    enumerator.enumerate()

    // Printer set up.
    val writer = new PrintWriter(new File("transform-email.txt"))  //specify the file path
    val results =
      enumerator.computeResults().map(e => mkCodeMultipleCtx(ctx, e)).toSet
      //enumerator.valueSpace.flatMap(_._2).map(e => mkCodeMultipleCtx(ctx, e)).toSet
    results.foreach(writer.println)
    writer.close()
  }

  test("Extract Area Code") {
    val vocab = Vocab(List(
      Var("columnName"),
      // Var("sep"),
      Str(" "),
      Str("-"),
      Num(0),
      Num(1),
      Num(2),
      Num(3),
      Num(4)
    ),
      mutable.Map(
        "SPLIT" -> 0,
        "LENGTH" -> 0,
        "STRINDEX" -> 0,
        "ARRINDEX" -> 0,
        "CONCAT" -> 0,
        "STRSUBSTR" -> 0,
        "STRREPLACE" -> 0,
        "LENGTH" -> 0,
      )
    )

    val ctx = List(
      Map(
        Var("columnName").x -> StringV("425-829-5512"),
        Var("sep").x -> StringV(","),
      ),
    )

    val typeCtx = Map(
      Var("columnName").x -> StringTy,
      Var("sep").x -> StringTy,
    )

    val res = List(
      StringV("425"),
    )

    val enumerator = new Enumeration(vocab, typeCtx, ctx, res, 7)
    enumerator.initProgramBank()
    enumerator.enumerate()

    // Printer set up.
    val writer = new PrintWriter(new File("extract-area-code.txt"))  //specify the file path
    val results =
      enumerator.computeResults().map(e => mkCodeMultipleCtx(ctx, e))
        .distinct
        .sortWith(_.length < _.length)
    results.foreach(writer.println)
    writer.close()

    // Print top-k programs.
    val topCandidates = enumerator.topKCandidates.slice(0,10).map(e => (mkCodeMultipleCtx(ctx, e), eval(ctx, e)))
    for (x <- topCandidates) {
      print(x)
    }
  }


  test("Extract Last name") {
    val vocab = Vocab(List(
      Var("columnName"),
      // Var("sep"),
//      Str(","),
//      Str("."),
      Str(" "),
      Num(0),
      Num(1),
      Num(2),
      Num(3)
    ),
      mutable.Map(
        "SPLIT" -> 0,
//        "LENGTH" -> 0,
        "STRINDEX" -> 0,
        "ARRINDEX" -> 0,
        "CONCAT" -> 0,
        "STRSUBSTR" -> 0,
//        "STRREPLACE" -> 0,
//        "LENGTH" -> 0,
      )
    )

    val ctx = List(
      Map(
        Var("columnName").x -> StringV("Greta Hermansson"),
      ),
    )

    val typeCtx = Map(
      Var("columnName").x -> StringTy,
    )

    val res = List(
      StringV("Hermansson"),
    )

    val enumerator = new Enumeration(vocab, typeCtx, ctx, res, 8)
    enumerator.initProgramBank()
    enumerator.enumerate()

    // Printer set up.
    val writer = new PrintWriter(new File("extract-last-name.txt"))  //specify the file path
    val results =
      enumerator.computeResults().map(e => mkCodeMultipleCtx(ctx, e))
        .distinct
        .sortWith(_.length < _.length)
    results.foreach(res => { writer.println(res + ",") })
    writer.close()
  }


  test("Add Bracket") {
    val vocab = Vocab(List(
      Var("SSN"),
      // Var("sep"),
      Str("]"),
      Str("["),
      Str(" "),
      Num(0),
      Num(1),
      Num(2),
      Num(3)
    ),
      mutable.Map(
        //"SPLIT" -> 0,
        "STRINDEX" -> 0,
        "ARRINDEX" -> 0,
        "CONCAT" -> 0,
        "STRSUBSTR" -> 0,
        "LENGTH" -> 0,
      )
    )

    val ctx = List(
      Map(
        Var("SSN").x -> StringV("1739302"),
      ),
    )

    val typeCtx = Map(
      Var("SSN").x -> StringTy,
    )

    val res = List(
      StringV("[1739302]"),
    )

    val enumerator = new Enumeration(vocab, typeCtx, ctx, res, 8)
    enumerator.initProgramBank()
    enumerator.enumerate()

    // Printer set up.
    val writer = new PrintWriter(new File("add-brackets.txt"))  //specify the file path
    val results =
      enumerator.computeResults().map(e => mkCodeMultipleCtx(ctx, e))
        .distinct
        .sortWith(_.length < _.length)
      //enumerator.valueSpace.flatMap(_._2).map(e => mkCodeMultipleCtx(ctx, e)).toSet
    results.foreach(res => { writer.println(res + ",") })
    writer.close()
  }

  test("Extract first name") {
    val vocab = Vocab(List(
      Var("name"),
      Str(" "),
      Num(0),
      Num(1),
      Num(2),
      Num(4)
    ),
      mutable.Map(
        "SPLIT" -> 0,
        "STRSUBSTR" -> 0,
        "LENGTH" -> 0,
        "STRINDEX" -> 0,
        "ARRINDEX" -> 0,
        "CONCAT" -> 0,
      )
    )

    val ctx = List(
      Map(
        Var("name").x -> StringV("Alice Smith"),
      ),
      Map(
        Var("name").x -> StringV("Benjamin H. Baker"),
      )
    )

    val typeCtx = Map(
      Var("name").x -> StringTy,
    )

    val res = List(
      StringV("Alice"),
      StringV("Benjamin"),
    )

    val enumerator = new Enumeration(vocab, typeCtx, ctx, res, 7)
    enumerator.initProgramBank()
    enumerator.enumerate()
//    enumerator.topKCandidates.slice(0,10).map(e => {
//      (mkCodeMultipleCtx(ctx, e), eval(ctx, e).toList)
//    }).foreach(println)

    // Get top-k
    val topCandidates = enumerator.sortValueSpace().map(e => (mkCodeMultipleCtx(ctx, e), eval(ctx, e)))
    for (x <- topCandidates) {
      println(x)
    }

    // Printer set up.
    val writer = new PrintWriter(new File("extract-first-name.txt"))  //specify the file path
    val results =
      enumerator.computeResults().map(e => mkCodeMultipleCtx(ctx, e))
        .distinct
        .sortWith(_.length < _.length)
    results.foreach(res => { writer.println(res + ",") })
    writer.close()
  }
}
