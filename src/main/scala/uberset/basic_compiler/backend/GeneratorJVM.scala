/*
  Author: uberset
  Date: 2015-11-23
  Licence: GPL v2
*/

package uberset.basic_compiler.backend

import uberset.basic_compiler._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


/**
  * Code generator for Java Virtual Machine.
  */
object GeneratorJVM {

    case class Status(
        className: String,
        var dataCount: Int = 0,
        var lblCount: Int = 0,
        var varNames: Set[String] = Set(),
        var arraySizes: mutable.Map[String, Int] = mutable.HashMap[String, Int](),
        code: ListBuffer[String] = ListBuffer[String]()
    )

    def generate(p: Program, className: String): Seq[String] = {
        val s = Status(className)
        programm(p, s)
        s.code.prepend(prelude(s))
        s.code.append(end())
        s.code
    }

    def programm(prog: Program, s: Status): Unit = {
        for(l <- prog.lines) {
            line(l, s)
        }
    }

    def line(lin: Line, s: Status): Unit = {
        val Line(nrOption, stm) = lin
        nrOption.map{ nr: Int =>
            val lbl = lineLabel(nr)
            s.code.append(s"$lbl:\n") // linenumber as label
        }
        statement(stm, s)
    }

    def statement(stm: Statement, s: Status): Unit = {
        stm match {
            case stm: Print  => stmPrint (stm, s)
            case stm: Let    => stmLet   (stm, s)
            case stm: Goto   => stmGoto  (stm, s)
            case stm: Gosub  => stmGosub (stm, s)
            case stm: Return => stmReturn(stm, s)
            case stm: If     => stmIf    (stm, s)
            case stm: Rem    => ()
            case stm: Input  => stmInput (stm, s)
            case stm: Dim    => stmDim   (stm, s)
            case stm: For    => stmFor   (stm, s)
            case stm: Next   => stmNext  (stm, s)
        }
    }

    def stmNext(nxt: Next, s: Status): Unit = {
        ???
    }

    def stmFor(f: For, s: Status): Unit = {
        ???
    }

    def stmDim(dim: Dim, s: Status): Unit = {
        val Dim(id, upper) = dim
        s.arraySizes.get(id) match {
            case Some(_) => throw new Exception(s"Array $id was already dimensioned.")
            case None => s.arraySizes.put(id, upper+1)
        }
    }

    def stmInput(inp: Input, s: Status): Unit = {
        val vari = inp.variable
        s.code.append(
            """invokestatic  java/lang/System/console()Ljava/io/Console;
              |invokevirtual java/io/Console/readLine()Ljava/lang/String;
              |invokestatic  java/lang/Integer/parseInt(Ljava/lang/String;)I
              |""".stripMargin
        )
        setVari(vari, s)
/*        s.code.append(
            "\t\tcall putln\n"
        )*/
    }

    def stmIf(stm: If, s: Status): Unit = {
        val Condition(a, rel, b) = stm.cond
        val nr = stm.line

        val opcode = rel match {
            case EQ() => "if_icmpeq"
            case NE() => "if_icmpne"
            case GT() => "if_icmpgt"
            case LT() => "if_icmplt"
            case GE() => "if_icmpge"
            case LE() => "if_icmple"
        }
        val lbl = lineLabel(nr)

        evalExpression(a, s)
        evalExpression(b, s)
        // pop values from stack, cmp them and jump on condition
        s.code.append(
            s"$opcode $lbl\n"
        )
    }

    def stmReturn(ret: Return, s: Status): Unit = {
        s.code.append(s"ret\n")
    }

    def stmGosub(go: Gosub, s: Status): Unit = {
        val nr = go.nr
        val lbl = lineLabel(nr)
        s.code.append(s"jsr $lbl\n")
    }

    def stmGoto(go: Goto, s: Status): Unit = {
        val nr = go.nr
        val lbl = lineLabel(nr)
        s.code.append(s"goto $lbl\n")
    }

    def lineLabel(nr: Int) = s"LINE_$nr"

    def stmLet(let: Let, s: Status): Unit = {
        val Let(vari, expr) = let
        evalExpression(expr, s) // push value on stack
        // pop value from stack and store in variable
        setVari(vari, s)
    }

    def setVari(vari: Variable, s: Status): Unit = {
        // get value from stack and store in variable
        vari match {
            case Variable(id, None) =>
                s.code.append(s"putstatic ${s.className}/$id S\n"
                )
                s.varNames = s.varNames + id
            case Variable(id, Some(expr)) =>
                s.code.append(
                    s"""getstatic ${s.className}/$id [S
                       |swap
                       |""".stripMargin)
                evalExpression(expr, s)
                s.code.append(
                    """swap
                      |sastore
                      |""".stripMargin
                )
        }
    }

    def stmPrint(prt: Print, s: Status): Unit = {
        prt.arg match {
            case StringArg(str) => printString(str, s)
            case expr: Expression => printExpression(expr, s)
        }
    }

    def printExpression(expr: Expression, s: Status): Unit = {
        evalExpression(expr, s)
        printInteger(s)
    }

    def printInteger(s: Status): Unit = {
        // pop int from stack and print it
        s.code.append(
            s"""getstatic java/lang/System/out Ljava/io/PrintStream;
                |swap
                |invokevirtual java/io/PrintStream/println(I)V
                |""".stripMargin
        )
    }

    def evalExpression(expr: Expression, s: Status): Unit = {
        val Expression(ng, term, ops) = expr
        evalTerm(term, s)
        if(ng) neg(s)
        for((op, t) <- ops) evalTerm(op, t, s)
    }

    def evalTerm(op: AddOp, t: Term, s: Status): Unit = {
        evalTerm(t, s)
        op match {
            case Add() => add(s)
            case Sub() => sub(s)
        }
    }

    def evalTerm(term: Term, s: Status): Unit = {
        val Term(factor, ops) = term
        evalFactor(factor, s)
        for((op, f) <- ops) evalFactor(op, f, s)
    }

    def evalFactor(op: MulOp, f: Factor, s: Status): Unit = {
        evalFactor(f, s)
        op match {
            case Mul() => mul(s)
            case Div() => div(s)
        }
    }

    def evalFactor(factor: Factor, s: Status): Unit = {
        factor match {
            case IntValue(i) => evalInt(i, s)
            case Variable(id, None) => evalVar(id, s)
            case Variable(id, Some(expr)) => evalExpression(expr, s); evalArray(id, s)
            case expr: Expression => evalExpression(expr, s)
        }
    }

    def neg(s: Status): Unit = {
        // pop value from stack, neg it, push result to stack
        s.code.append("ineg\n")
    }

    def div(s: Status): Unit = {
        // pop values from stack, div them, push result to stack
        s.code.append("idiv\n")
    }

    def mul(s: Status): Unit = {
        // pop values from stack, mul them, push result to stack
        s.code.append("imul\n")
    }

    def sub(s: Status): Unit = {
        // pop values from stack, sub them, push result to stack
        s.code.append("isub\n")
    }

    def add(s: Status): Unit = {
        // pop values from stack, add them, push result to stack
        s.code.append("iadd\n")
    }

    def evalVar(id: String, s: Status): Unit = {
        // read variable from data section and push the value to the stack
        s.code.append(
            s"getstatic ${s.className}/$id S\n"
        )
        s.varNames = s.varNames + id
    }

    def evalArray(id: String, s: Status): Unit = {
        // get index from stack
        // read array from data section and push the value to the stack
        s.code.append(
            s"""getstatic ${s.className}/$id [S
               |swap
               |saload
               |""".stripMargin
        )
    }

    def evalInt(i: Int, s: Status): Unit = {
        // push i on the stack
        s.code.append(
            s"""sipush $i
               |""".stripMargin
        )
    }

    def printString(str: String, s: Status): Unit = {
        s.code.append(
            s"""getstatic java/lang/System/out Ljava/io/PrintStream;
               |ldc "$str"
               |invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
               |""".stripMargin
        )
    }

    def vars(s: Status): String = {
        // define all used variables and arrays as static class members
        val vars = for {
            id <- s.varNames
        } yield s".field static $id S\n"
        val arrs = for {
            id <- s.arraySizes.keys
        } yield s".field static $id [S\n"
        (vars ++ arrs).mkString("\n")
    }

    def arrs(s: Status): String = {
        // code to allocate static arrays
         """.method static <clinit>()V
           |   .limit stack  1
           |   .limit locals 0
           |""".stripMargin +
        (for{
            (id, size) <- s.arraySizes
        } yield
            s"""   sipush    $size
               |   newarray  short
               |   putstatic ${s.className}/$id [S
               |""".stripMargin).mkString +
         """   return
           |.end method
           |""".stripMargin
    }

    def end(): String = {
        """   return
          |.end method
          |""".stripMargin
    }

    def prelude(s: Status): String = {
        val fields = vars(s)
        val arrays = arrs(s)
        s""".class public ${s.className}
           |.super java/lang/Object
           |$fields
           |$arrays
           |.method public <init>()V
           |   aload_0
           |   invokenonvirtual java/lang/Object/<init>()V
           |   return
           |.end method
           |.method public static main([Ljava/lang/String;)V
           |   .limit locals 40
           |   .limit stack 10000
           |""".stripMargin
    }

}
