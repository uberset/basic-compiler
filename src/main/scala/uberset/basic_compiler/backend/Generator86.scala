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
        library(s)
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
            case stm: Print => stmPrint(stm, s)
            case stm: Let   => stmLet  (stm, s)
            case stm: Goto  => stmGoto (stm, s)
            case stm: If    => stmIf   (stm, s)
        }
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

    def stmGoto(go: Goto, s: Status): Unit = {
        val nr = go.nr
        val lbl = s"LINE_$nr"
        s.out.append(s"\t\tjmp $lbl\n")
    }

    def stmLet(let: Let, s: Status): Unit = {
        val Let(id, expr) = let
        evalExpression(expr, s) // push value on stack
        // pop value from stack and store in variable
        s.out.append(
            "\t\tpop ax\n",
            s"\t\tmov [VAR_$id], ax\n"
        )
        s.varNames = s.varNames + id
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
        expr match {
            case v: Value => evalValue(v, s)
            case b: BinOperation => evalBop(b, s)
            case b: UnOperation  => evalUop(b, s)
        }
    }

    def evalUop(uop: UnOperation, s: Status): Unit = {
        uop match {
            case Neg(a) => evalValue(a, s); neg(s)
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

    def evalBop(bop: BinOperation, s: Status): Unit = {
        bop match {
            case Add(a, b) => evalValue(a, s); evalValue(b, s); add(s)
            case Sub(a, b) => evalValue(a, s); evalValue(b, s); sub(s)
            case Mul(a, b) => evalValue(a, s); evalValue(b, s); mul(s)
            case Div(a, b) => evalValue(a, s); evalValue(b, s); div(s)
        }
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

    def evalValue(v: Value, s: Status): Unit = {
        v match {
            case IntValue(i) => evalInt(i, s)
            case Variable(id) => evalVar(id, s)
        }
    }

    def evalVar(id: String, s: Status): Unit = {
        // read variable from data section and push the value to the stack
        s.out.append(
            s"\t\tmov ax, [VAR_$id]\n",
            "\t\tpush ax\n"
        )
        s.varNames = s.varNames + id
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

    def library(s: Status) = {
        s.out.append(
"""
puti:	; put a signed integer (16 bit) to stdout
		; int in AX
		; AX, BX, CX, DX will be modified
		call int2decimal	; returns pointer to string in bx
		call puts
		ret

int2decimal:
		; convert a signed integer (16 bit) to a buffer
		; int in AX
		; AX, BX, CX, DX will be modified
		; buffer: CX
		; divisor: BX
		mov dl, '+'	; sign
		cmp	ax,0
		jge .unsigned
		neg ax
		mov dl, '-'
.unsigned:
		mov bx, .buffer
		mov [bx], dl	; sign
		mov cx, .endbuf-2
.next:	mov dx, 0
		mov bx, 10
		div bx	; ax = (dx, ax) / bx
				; dx = remainder
		mov bx, cx
		add dl, '0'
		mov [bx], dl	; digit
		dec cx
		cmp ax, 0
		jne .next
		; move sign if necessary
		; BX points to the first digit now
		mov dl, [.buffer]	; sign '+' or '-'
		cmp dl, '-'
		jne .end	; no '-'
		dec bx
		mov [bx], dl	; copy sign
.end:	ret
section .data
.buffer	db		"-", "12345", 0
.endbuf:
section .text

puts:	; put a string to stdout
		; string start address in BX
		; string must be terminated with null
		; AX, BX, DX will be modified
		mov dl,[bx]     ; load character
		cmp dl, 0
		jz  .end
		mov ah,2		; output char to stdout (ah: 02, dl: char)
		int 0x21		; DOS
		inc bx
		jmp puts
.end:	ret

putln:	; put CR LF to stdout
		mov bx, .line
		jmp puts
.line:	db 0x0A, 0x0D, 0
"""     )
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
