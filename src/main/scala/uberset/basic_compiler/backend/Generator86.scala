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
        out: ListBuffer[String] = ListBuffer[String]()
    )

    def generate(p: Program): Seq[String] = {
        val s = Status()
        prelude(s)
        programm(p, s)
        end(s)
        library(s)
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
            s.out.append(s"L$nr:\n") // linenumber as label
        }
        statement(stm, s)
    }

    def statement(stm: Statement, s: Status): Unit = {
        stm match {
            case p :Print => stmPrint(p, s)
            case _ => ???
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
        "\t\tcall puti\n"
        )
    }

    def evalExpression(expr: Expression, s: Status): Unit = {
        expr match {
            case v: Value => evalValue(v, s)
        }
    }

    def evalValue(v: Value, s: Status): Unit = {
        v match {
            case IntValue(i) => evalInt(i, s)
        }
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
