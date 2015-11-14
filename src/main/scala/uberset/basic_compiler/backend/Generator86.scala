/*
  Author: uberset
  Date: 2015-11-14
  Licence: GPL v2
*/

package uberset.basic_compiler.backend


import uberset.basic_compiler.Program

import scala.collection.mutable.ListBuffer


/**
  * Code generator for Intel 8086 compatible processors and MS-DOS compatible operating systems.
  */
object Generator86 {

    case class Status(
        out: ListBuffer[String] = ListBuffer[String]()
    )

    def generate(p: Program): Seq[String] = {
        val s = Status()
        prelude(s)
        // programm(p, s)
        end(s)
        library(s)
        s.out
    }

    def library(s: Status) = {
        ()
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
