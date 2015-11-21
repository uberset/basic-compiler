package uberset.basic_compiler

/*
  Author: uberset
  Date: 2015-11-09
  Licence: GPL v2
*/

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Interpreter {

    case class Status(
        lines: Seq[Line],
        var in: List[String],
        var lineIndex: Int = 0,
        var running: Boolean = true,
        variables: mutable.Map[String, Int] = mutable.HashMap[String, Int](),
        arrays: mutable.Map[String, Array[Int]] = mutable.HashMap[String, Array[Int]](),
        stack: mutable.Stack[Int] = mutable.Stack[Int](),
        out: ListBuffer[String] = ListBuffer[String]()
    )

    def run(p: Program, in: List[String]): Seq[String] = {
        val s = Status(p.lines, in)
        while(s.running) run(s)
        s.out
    }

    def run(s: Status): Unit = {
        val line = s.lines(s.lineIndex)
        runStm(line.stm, s)
        s.lineIndex += 1
        if(s.lineIndex >= s.lines.length) s.running = false
    }

    def runStm(stm: Statement, s: Status): Unit = {
        stm match {
            case stm: Print => run(stm, s)
            case stm: Goto => run(stm, s)
            case stm: Gosub => run(stm, s)
            case stm: Return => run(stm, s)
            case stm: Let => run(stm, s)
            case stm: If => run(stm, s)
            case stm: Rem => ()
            case stm: Input => run(stm, s)
            case stm: Dim => run(stm, s)
        }
    }

    def run(dim: Dim, s: Status): Unit = {
        val Dim(id, upper) = dim
        s.arrays.get(id) match {
            case Some(_) => fail(s"Array $id was already dimensioned.")
            case None => s.arrays.put(id, new Array[Int](upper+1))
        }
    }

    def run(input: Input, s: Status): Unit = {
        val vari = input.variable
        val string = s.in.head
        val valu = string.toInt
        s.in = s.in.tail
        setVariable(vari, valu, s)
    }

    def evalCond(c: Condition, s: Status): Boolean = {
        val Condition(e1, op, e2) = c
        op match {
            case LT() => evalExpr(e1, s) < evalExpr(e2, s)
            case GT() => evalExpr(e1, s) > evalExpr(e2, s)
            case EQ() => evalExpr(e1, s) == evalExpr(e2, s)
            case NE() => evalExpr(e1, s) != evalExpr(e2, s)
            case LE() => evalExpr(e1, s) <= evalExpr(e2, s)
            case GE() => evalExpr(e1, s) >= evalExpr(e2, s)
        }
    }

    def run(i: If, s: Status): Unit = {
        val If(c, nr) = i
        val v = evalCond(c, s)
        if(v) goto(nr, s)
    }

    def run(l: Let, s: Status): Unit = {
        val Let(vari, expr) = l
        val valu = evalExpr(expr, s)
        setVariable(vari, valu, s)
    }

    def run(p: Print, s: Status): Unit = {
        s.out.append(evalPrintArg(p.arg, s), "\n")
    }

    def evalPrintArg(arg: PrintArgument, s: Status): String = {
        arg match {
            case StringArg(s) => s
            case expr: Expression => evalExpr(expr, s).toString
        }
    }

    def evalExpr(expr: Expression, s: Status): Int = {
        val Expression(neg, term, ops) = expr
        var v = evalTerm(term, s)
        if(neg) v = (-v)
        for((op, t) <- ops) v = evalTerm(v, op, t, s)
        v
    }

    def evalTerm(v: Int, op: AddOp, t: Term, s: Status): Int = {
        val v2 = evalTerm(t, s)
        op match {
            case Add() => v + v2
            case Sub() => v - v2
        }
    }

    def evalTerm(term: Term, s: Status): Int = {
        val Term(factor, ops) = term
        var v = evalFactor(factor, s)
        for((op, f) <- ops) v = evalFactor(v, op, f, s)
        v
    }

    def evalFactor(v: Int, op: MulOp, f: Factor, s: Status): Int = {
        val v2 = evalFactor(f, s)
        op match {
            case Mul() => v * v2
            case Div() => v / v2
        }
    }

    def evalFactor(factor: Factor, s: Status): Int = {
        factor match {
            case IntValue(i) => i
            case v: Variable => evalVariable(v, s)
            case expr: Expression => evalExpr(expr, s)
        }
    }

    def evalVariable(v: Variable, s: Status): Int = {
        val Variable(id, sub) = v
        s.arrays.get(id) match {
            // array found
            case Some(values) =>
                sub match {
                    case Some(expr) => values(evalExpr(expr, s))
                    case None => fail(s"Subscript for array $id expected.").asInstanceOf[Int] // the compiler complains without cast :(
                }
            // array not found
            case None =>
                sub match {
                    case Some(expr) => fail(s"Array $id must be dimensioned.").asInstanceOf[Int] // the compiler complains without cast :(
                    case None => s.variables.getOrElse(id, 0)
                }
        }
    }

    def setVariable(v: Variable, value: Int, s: Status): Unit = {
        val Variable(id, sub) = v
        s.arrays.get(id) match {
            // array found
            case Some(values) =>
                sub match {
                    case Some(expr) => values(evalExpr(expr, s)) = value
                    case None => fail(s"Subscript for array $id expected.")
                }
            // array not found
            case None =>
                sub match {
                    case Some(expr) => fail(s"Array $id must be dimensioned.")
                    case None => s.variables.put(id, value)
                }
        }
    }

    def run(g: Return, s: Status): Unit = {
        s.lineIndex = s.stack.pop()
    }

    def run(g: Gosub, s: Status): Unit = {
        s.stack.push(s.lineIndex)
        goto(g.nr, s)
    }

    def run(g: Goto, s: Status): Unit = goto(g.nr, s)

    def goto(nr: Int, s: Status): Unit = {
        val index =  findLineIndex(nr, s.lines)
        s.lineIndex = index - 1
    }

    def findLineIndex(nr: Int, lines: Seq[Line]): Int = {
        for(i <- 0 until lines.length) {
            if(lines(i).nr == Some(nr)) return i
        }
        throw new Exception(s"Line number $nr not found.")
    }

    def fail(msg: String): Null = {
        throw new Exception(msg)
    }
}
