package uberset.basic_compiler

/*
  Author: uberset
  Date: 2015-11-09
  Licence: GPL v2
*/

import scala.collection.mutable.ListBuffer

object Interpreter {

    case class Status(
        lines: Seq[Line],
        var lineIndex: Int = 0,
        var running: Boolean = true,
        out: ListBuffer[String] = new ListBuffer[String]
    )

    def run(p: Program): Seq[String] = {
        val s = Status(p.lines)
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
        }
    }

    def run(p: Print, s: Status): Unit = {
        s.out.append(p.string, "\n")
    }

    def run(g: Goto, s: Status): Unit = {
        val index =  findLineIndex(g.nr, s.lines)
        s.lineIndex = index - 1
    }

    def findLineIndex(nr: Int, lines: Seq[Line]): Int = {
        for(i <- 0 until lines.length) {
            if(lines(i).nr == Some(nr)) return i
        }
        throw new Exception(s"Line number $nr not found.")
    }

}
