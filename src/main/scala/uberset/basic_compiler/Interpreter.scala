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
            case stm: Let => run(stm, s)
            case stm: If => run(stm, s)
            case stm: Rem => ()
            case stm: Input => run(stm, s)
        }
    }

    def run(input: Input, s: Status): Unit = {
        val id = input.variable
        val string = s.in.head
        val v = string.toInt
        s.in = s.in.tail
        s.variables.put(id, v)
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
        val Let(id, expr) = l
        val v = evalExpr(expr, s)
        s.variables.put(id, v)
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
            case Variable(id) => s.variables.getOrElse(id, 0)
            case expr: Expression => evalExpr(expr, s)
        }
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

}
