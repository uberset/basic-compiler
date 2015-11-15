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
            case _ => ???
        }
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
