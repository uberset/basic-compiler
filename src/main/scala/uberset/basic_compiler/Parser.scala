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
// letter = 'A' ... 'Z'

// program = line* ;
case class Program(lines: Seq[Line])

// line = [integer] statement ;
case class Line(nr: Option[Int], stm: Statement)
// statement = print | goto | let ;
sealed abstract class Statement

// let = 'LET' variable '=' expression ;
case class Let(variable: String, expression: Expression) extends Statement

// print = 'PRINT' printargument ;
case class Print(arg: PrintArgument) extends Statement

// printargument = string | expression ;
sealed abstract class PrintArgument
case class StringArg(string: String) extends PrintArgument

// expression = binoperation | unoperation | value ;
sealed abstract class Expression extends PrintArgument

// value = intvalue | variable ;
sealed abstract class Value extends Expression

// intvalue = integer ;
case class IntValue(value: Int) extends Value

// variable = letter digit? ;
case class Variable(name: String) extends Value

// binoperation = value ( '+' | '-' | '*' | '/' ) value ;
sealed abstract class BinOperation extends Expression
case class Add(v1: Value, v2: Value) extends BinOperation
case class Sub(v1: Value, v2: Value) extends BinOperation
case class Mul(v1: Value, v2: Value) extends BinOperation
case class Div(v1: Value, v2: Value) extends BinOperation

// unoperation = '-' value ;
sealed abstract class UnOperation extends Expression
case class Neg(v: Value) extends UnOperation

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
        else if(s.startsWith(LET)) stmLet(s)
        else fail(s"Statement expected at: $s")
    }

    val PRINT = "PRINT"
    val GOTO = "GOTO"
    val LET = "LET"

    def stmLet(s: String): (Let, String) = {
        val s1 = require(LET, s)
        val (v, s2) = variable(s1)
        val s3 = require('=', s2)
        val (expr, rest) = expression(s3)
        (Let(v, expr), rest)
    }

    def stmPrint(s: String): (Print, String) = {
        val s1 = require(PRINT, s)
        if(!s1.isEmpty && s1.charAt(0) == '\"') {
            val (str, rest) = string(s1)
            (Print(str), rest)
        } else {
            val (expr, rest) = expression(s1)
            (Print(expr), rest)
        }
    }

    def expression(s: String): (Expression, String) = {
        if(!s.isEmpty && s.charAt(0) == '-') {
            neg(s)
        } else {
            val (v, rest) = value(s)
            tryBinopRight(v, rest)
        }
    }

    // returns v1 or (v1 op v2)
    def tryBinopRight(v1: Value, s: String): (Expression, String) = {
        if(!s.isEmpty) {
            val c = s.charAt(0)
            c match {
                case '+' | '-' | '*' | '/' => binOperation(v1, c, value(s.substring(1)))
                case _ => (v1, s)
            }
        } else {
            (v1, s)
        }
    }

    def binOperation(v1: Value, c: Char, tuple: (Value, String)): (BinOperation, String) = {
        val (v2, rest) = tuple
        c match {
            case '+' => (Add(v1, v2), rest)
            case '-' => (Sub(v1, v2), rest)
            case '*' => (Mul(v1, v2), rest)
            case '/' => (Div(v1, v2), rest)
        }
    }

    def neg(s: String): (Neg, String) = {
        val s1 = require('-', s)
        val (v, rest) = value(s1)
        (Neg(v), rest)
    }

    def value(s: String): (Value, String) = {
        if(!s.isEmpty && s.charAt(0).isDigit) {
            val (i, rest) = integer(s)
            (IntValue(i), rest)
        } else {
            val (v, rest) = variable(s)
            (Variable(v), rest)
        }
    }

    def variable(s: String): (String, String) = {
        val (l, rest) = letter(s)
        val (d, rest1) = tryDigit(rest)
        d match {
            case Some(c) => (List(l,  c).mkString, rest1)
            case None    => (l.toString, rest)
        }
    }

    def tryDigit(s: String): (Option[Char], String) = {
        if(!s.isEmpty && s.charAt(0).isDigit) {
            (Some(s.charAt(0)), s.substring(1))
        } else {
            (None, s)
        }
    }

    def letter(s: String): (Char, String) = {
        if(!s.isEmpty) {
            val c = s.charAt(0)
            if(c>='A' && c<='Z')
                return (c, s.substring(1))
        }
        fail("Uppercase letter required")
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

object Line {
    def apply(stm: Statement): Line = Line(None, stm)
    def apply(nr: Int, stm: Statement): Line = Line(Some(nr), stm)
}

object Print {
    def apply(string: String): Print = Print(StringArg(string))
}
