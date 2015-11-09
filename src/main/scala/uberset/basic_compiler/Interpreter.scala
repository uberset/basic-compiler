package uberset.basic_compiler

/*
  Author: uberset
  Date: 2015-11-09
  Licence: GPL v2
*/

import scala.collection.mutable.ListBuffer

object Interpreter {

    def run(p: Program): Seq[String] = {
        val out = new ListBuffer[String]
        p match {
            case Program(lines) => lines.foreach { line => run(line, out) }
        }
        out
    }

    def run(l: Line, out: ListBuffer[String]): Unit = {
        l match {
            case Line(p) => run(p, out)
        }
    }

    def run(p: Print, out: ListBuffer[String]): Unit = {
        p match {
            case Print(str) => out.append(str, "\n")
        }
    }

}
