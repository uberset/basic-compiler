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
// digit = '0' ... '1'

// program = line* ;
case class Program(lines: Seq[Line])

// line = [integer] print ;
case class Line(nr: Option[Int], p: Print)
object Line {
    def apply(p: Print): Line = Line(None, p)
    def apply(nr: Int, p: Print): Line = Line(Some(nr), p)
}

// print = 'PRINT' string ;
case class Print(string: String)

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
        val p = stmPrint(rest)
        Line(nr, p)
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

    val PRINT = "PRINT"

    def stmPrint(s: String): Print = {
        val s1 = require(PRINT, s)
        val (str, rest) = string(s1)
        if(!rest.isEmpty) fail(s"Unparsed text after statement: $rest")
        Print(str)
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
