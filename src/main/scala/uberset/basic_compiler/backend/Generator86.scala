/*
  Author: uberset
  Date: 2015-11-14
  Licence: GPL v2
*/

package uberset.basic_compiler.backend


import uberset.basic_compiler._

import scala.collection.mutable.ListBuffer


/**
  * Code generator for Intel 8086 compatible processors and MS-DOS compatible operating systems.
  */
object Generator86 {

    case class Status(
         var dataCount: Int = 0,
         var varNames: Set[String] = Set(),
         out: ListBuffer[String] = ListBuffer[String]()
    )

    def generate(p: Program): Seq[String] = {
        val s = Status()
        prelude(s)
        programm(p, s)
        end(s)
        Library86.library(s.out)
        vars(s)
        s.out
    }

    def programm(prog: Program, s: Status): Unit = {
        for(l <- prog.lines) {
            line(l, s)
        }
    }

    def line(lin: Line, s: Status): Unit = {
        val Line(nrOption, stm) = lin
        nrOption.map{ nr: Int =>
            s.out.append(s"LINE_$nr:\n") // linenumber as label
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
        }
    }

    def stmDim(dim: Dim, s: Status): Unit = {
        val Dim(id, upper) = dim
        val lbl = "ARR_" + id + ":"
        s.out.append(
            "section .data\n",
            s"$lbl\t\ttimes ${upper+1} dw 0\n",
            "section .text\n"
        )
    }

    def stmInput(inp: Input, s: Status): Unit = {
        val vari = inp.variable
        s.out.append(
            "\t\tcall getsbuff\n",
            "\t\tcall string2int\n",
            "\t\tpush ax\n"
        )
        setVari(vari, s)
        s.out.append(
            "\t\tcall putln\n"
        )
    }

    def stmIf(stm: If, s: Status): Unit = {
        val Condition(a, rel, b) = stm.cond
        val nr = stm.line

        val opcode = rel match {
            case EQ() => "je"
            case NE() => "jne"
            case GT() => "jgt"
            case LT() => "jlt"
            case GE() => "jge"
            case LE() => "jle"
        }
        val lbl = s"LINE_$nr"

        evalExpression(a, s)
        evalExpression(b, s)
        // pop values from stack, cmp them and jump on condition
        s.out.append(
            "\t\tpop bx\n",
            "\t\tpop ax\n",
            "\t\tcmp ax, bx\n",
            s"\t\t$opcode $lbl\n"
        )
    }

    def stmReturn(ret: Return, s: Status): Unit = {
        s.out.append(s"\t\tret\n")
    }

    def stmGosub(go: Gosub, s: Status): Unit = {
        val nr = go.nr
        val lbl = s"LINE_$nr"
        s.out.append(s"\t\tcall $lbl\n")
    }

    def stmGoto(go: Goto, s: Status): Unit = {
        val nr = go.nr
        val lbl = s"LINE_$nr"
        s.out.append(s"\t\tjmp $lbl\n")
    }

    def stmLet(let: Let, s: Status): Unit = {
        val Let(vari, expr) = let
        evalExpression(expr, s) // push value on stack
        // pop value from stack and store in variable
        setVari(vari, s)
    }

    def setVari(vari: Variable, s: Status): Unit = {
        // pop value from stack and store in variable
        vari match {
            case Variable(id, None) =>
                s.out.append(
                    "\t\tpop ax\n",
                    s"\t\tmov [VAR_$id], ax\n"
                )
                s.varNames = s.varNames + id
            case Variable(id, Some(expr)) =>
                evalExpression(expr, s)
                s.out.append(
                    "\t\tpop si\n",
                    "\t\tpop ax\n",
                    s"\t\tmov [ARR_$id+si], ax\n"
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
        s.out.append(
            "\t\tpop ax\n",
            "\t\tcall puti\n",
            "\t\tcall putln\n"
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
        s.out.append(
            "\t\tpop ax\n",
            "\t\tneg ax\n",
            "\t\tpush ax\n"
        )
    }

    def div(s: Status): Unit = {
        // pop values from stack, div them, push result to stack
        s.out.append(
            "\t\tpop bx\n",
            "\t\tpop ax\n",
            "\t\tcwd\n",     // sign extension from AX into DX
            "\t\tidiv bx\n", // AX = (DX AX) / BX
            "\t\tpush ax\n"
        )
    }

    def mul(s: Status): Unit = {
        // pop values from stack, mul them, push result to stack
        s.out.append(
            "\t\tpop bx\n",
            "\t\tpop ax\n",
            "\t\timul ax, bx\n",
            "\t\tpush ax\n"
        )
    }

    def sub(s: Status): Unit = {
        // pop values from stack, sub them, push result to stack
        s.out.append(
            "\t\tpop bx\n",
            "\t\tpop ax\n",
            "\t\tsub ax, bx\n",
            "\t\tpush ax\n"
        )
    }

    def add(s: Status): Unit = {
        // pop values from stack, add them, push result to stack
        s.out.append(
            "\t\tpop bx\n",
            "\t\tpop ax\n",
            "\t\tadd ax, bx\n",
            "\t\tpush ax\n"
        )
    }

    def evalVar(id: String, s: Status): Unit = {
        // read variable from data section and push the value to the stack
        s.out.append(
            s"\t\tmov ax, [VAR_$id]\n",
            "\t\tpush ax\n"
        )
        s.varNames = s.varNames + id
    }

    def evalArray(id: String, s: Status): Unit = {
        // pop index from stack
        // read array from data section and push the value to the stack
        s.out.append(
            "\t\tpop si\n",
            s"\t\tmov ax, [ARR_$id+si]\n",
            "\t\tpush ax\n"
        )
    }

    def evalInt(i: Int, s: Status): Unit = {
        // push i on the stack
        s.out.append(
            s"\t\tmov ax, $i\n",
            "\t\tpush ax\n"
        )
    }

    def printString(str: String, s: Status): Unit = {
        val label = dataString(str, s)
        s.out.append(
            s"\t\tmov bx, $label\n",
            "\t\tcall puts\n",
            "\t\tcall putln\n"
        )
    }

    def dataString(str: String, s: Status): String = {
        val nr = s.dataCount; s.dataCount = nr+1
        val lbl = s"T$nr"
        s.out.append(
            "section .data\n",
            lbl+":\tdb\t\""+str+"\",0\n", // define string data with label and terminate with 0
            "section .text\n"
        )
        lbl
    }

    def vars(s: Status) = {
        if(!s.varNames.isEmpty) {
            // define all used variables in the data section
            s.out.append("section .data\n")
            for (id <- s.varNames) {
                val lbl = "VAR_" + id + ":"
                s.out.append(s"$lbl\t\tdw 0\n")
            }
            s.out.append("section .text\n")
        }
    }

    def end(s: Status) = {
        s.out.append(
            "\t\tmov ax,0x4c00\n",
            "\t\tint 0x21\n"
        )
    }

    def prelude(s: Status) = {
        s.out.append("\t\torg 100h\n")
    }

}
