package uberset.basic_compiler

import java.io.{BufferedReader, StringReader}

import scala.collection.mutable.ListBuffer

/*
  Author: uberset
  Date: 2015-11-09
  Licence: GPL v2
*/


// string = '"' string-char* '"' ;
// string-char = ? any character except '"' and newline> ? ;
// integer = digit+
// digit = '0' ... '9'

// program = line* ;
case class Program(lines: Seq[Line])

// line = [integer] statement ;
case class Line(nr: Option[Int], stm: Statement)
object Line {
    def apply(stm: Statement): Line = Line(None, stm)
    def apply(nr: Int, stm: Statement): Line = Line(Some(nr), stm)
}

// statement = print | goto ;
sealed abstract class Statement

// print = 'PRINT' string ;
case class Print(string: String) extends Statement

// goto = 'GOTO' integer ;
case class Goto(nr: Int) extends Statement

object Parser {

    def parse(text: String): Program = parse(new BufferedReader(new StringReader(text)))

    def parse(in: BufferedReader): Program = program(in)

    def program(in: BufferedReader): Program = {
        val lines = new ListBuffer[Line]
        var s: String = null
        do {
            s = in.readLine()
            if(s != null) {
                val l = line(s)
                lines.append(l)
            }
        } while (s != null)
        Program(lines)
    }

    // removes all spaces outside outside quotes
    def stripSpaces(s: String): String = {
        val s1 = " "+s+" " // workaround for strange split method
        s1.split('\"')
        .zipWithIndex
        .map{ case (s,i) => if(i%2==0) s.replaceAll(" ", "") else s }
        .mkString("\"")
    }

    def line(s: String): Line = {
        val s1 = stripSpaces(s)
        val (nr, rest) = tryInteger(s1)
        val (stm, r1) = statement(rest)
        if(!r1.isEmpty) fail(s"Unparsed text after statement: $r1")
        Line(nr, stm)
    }

    def integer(s: String): (Int, String) = {
        tryInteger(s) match {
            case (Some(nr), rest) => (nr, rest)
            case (None, rest) => fail(s"Integer is required at: '$s'")
        }
    }

    def tryInteger(s: String): (Option[Int], String) = {
        val numStr = s.takeWhile(_.isDigit)
        if(numStr.isEmpty) {
            (None, s)
        } else {
            val num = numStr.toInt
            val rest = s.substring(numStr.length)
            (Some(num), rest)
        }
    }

    def statement(s: String): (Statement, String) = {
        if(s.startsWith(PRINT)) stmPrint(s)
        else if(s.startsWith(GOTO)) stmGoto(s)
        else fail(s"Statement expected at: $s")
    }

    val PRINT = "PRINT"
    val GOTO = "GOTO"

    def stmPrint(s: String): (Print, String) = {
        val s1 = require(PRINT, s)
        val (str, rest) = string(s1)
        (Print(str), rest)
    }

    def stmGoto(s: String): (Goto, String) = {
        val s1 = require(GOTO, s)
        val (nr, rest) = integer(s1)
        (Goto(nr), rest)
    }

    def string(s: String): (String, String) = {
        val s1 = require('"', s)
        val str = s1.takeWhile(_ != '\"')
        val end = s1.substring(str.length)
        val rest = require('"', end)
        (str, rest)
    }

    def require(c: Char, s: String): String = {
        if(!s.isEmpty && s.charAt(0)==c)
            s.substring(1)
        else
            fail(s"'$c' is required at: '$s'")
    }

    def require(text: String, s: String): String = {
        if(s.startsWith(text))
            s.substring(text.length)
        else
            fail(s"'$text' is required at: '$s'")
    }

    def fail(msg: String): Null = {
        throw new Exception(msg)
    }

}
