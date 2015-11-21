package uberset.basic_compiler

import java.io.{BufferedReader, StringReader}

import scala.collection.mutable.ListBuffer

/*
  Author: uberset
  Date: 2015-11-09
  Licence: GPL v2
*/


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
        else if(s.startsWith(GOSUB)) stmGosub(s)
        else if(s.startsWith(RETURN)) stmReturn(s)
        else if(s.startsWith(LET)) stmLet(s)
        else if(s.startsWith(IF)) stmIf(s)
        else if(s.startsWith(REM)) stmRem(s)
        else if(s.startsWith(INPUT)) stmInput(s)
        else if(s.startsWith(DIM)) stmDim(s)
        else fail(s"Statement expected at: $s")
    }

    val PRINT = "PRINT"
    val GOTO = "GOTO"
    val GOSUB = "GOSUB"
    val RETURN = "RETURN"
    val LET = "LET"
    val IF = "IF"
    val THEN = "THEN"
    val REM = "REM"
    val INPUT = "INPUT"
    val DIM = "DIM"

    def stmDim(s: String): (Dim, String) = {
        val s1 = require(DIM, s)
        val (id, s2) = identifier(s1)
        val s3 = require('(', s2)
        val (upper, s4) = integer(s3)
        val rest = require(')', s4)
        (Dim(id, upper), rest)
    }

    def stmInput(s: String): (Input, String) = {
        val s1 = require(INPUT, s)
        val (id, rest) = variable(s1)
        (Input(id), rest)
    }

    def stmRem(s: String): (Rem, String) = {
        val rest = require(REM, s)
        // ignore the rest
        (Rem(), "")
    }

    def stmIf(s: String): (If, String) = {
        val s1 = require(IF, s)
        val (c, s2) = condition(s1)
        val s3 = require(THEN, s2)
        val (nr, s4) = integer(s3)
        (If(c, nr), s4)

    }

    def condition(s: String): (Condition, String) = {
        val (e1, s1) = expression(s)
        val (op, s2) = relOp(s1)
        val (e2, s3) = expression(s2)
        (Condition(e1, op, e2), s3)
    }

    def relOp(s: String): (RelOp, String) = {
        if(s.startsWith("<>")) (NE(), s.substring(2))
        else if(s.startsWith(">=")) (GE(), s.substring(2))
        else if(s.startsWith("<=")) (LE(), s.substring(2))
        else if(s.startsWith(">")) (GT(), s.substring(1))
        else if(s.startsWith("<")) (LT(), s.substring(1))
        else if(s.startsWith("=")) (EQ(), s.substring(1))
        else fail("Relational operation expected at: $s")
    }

    def trySubscript(s: String): (Option[Expression], String) = {
        if(!s.isEmpty && s.charAt(0) == '(') {
            val (expr, s1) = expression(s.substring(1))
            require(')', s1)
            (Some(expr), s1.substring(1))
        } else {
            (None, s)
        }
    }

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
        val neg = !s.isEmpty && s.charAt(0) == '-'
        val s1 = if(neg) s.substring(1) else s
        val (t, s2) = term(s1)
        val (ops, s3) = opTerms(s2)
        (Expression(neg, t, ops), s3)
    }

    def opTerms(s: String): (List[(AddOp, Term)], String) = {
        if(!s.isEmpty) {
            val c = s.charAt(0)
            c match {
                case '+' | '-'  => {
                    val op = addOperation(c)
                    val (t, s1) = term(s.substring(1))
                    val (list, rest) = opTerms(s1)
                    ((op, t) +: list, rest)
                }
                case _ => (List(), s)
            }
        } else {
            (List(), s)
        }
    }

    def addOperation(c: Char): AddOp = c match {
        case '+' => Add()
        case '-' => Sub()
    }

    def term(s: String): (Term, String) = {
        val (f, s1) = factor(s)
        val (ops, s2) = opFactors(s1)
        (Term(f, ops), s2)
    }

    def opFactors(s: String): (List[(MulOp, Factor)], String) = {
        if(!s.isEmpty) {
            val c = s.charAt(0)
            c match {
                case '*' | '/'  => {
                    val op = mulOperation(c)
                    val (f, s1) = factor(s.substring(1))
                    val (list, rest) = opFactors(s1)
                    ((op, f) +: list, rest)
                }
                case _ => (List(), s)
            }
        } else {
            (List(), s)
        }
    }

    def mulOperation(c: Char): MulOp = c match {
        case '*' => Mul()
        case '/' => Div()
    }

    def factor(s: String): (Factor, String) = {
        if(!s.isEmpty && s.charAt(0) == '(') {
            val (expr, s1) = expression(s.substring(1))
            val rest = require(')', s1)
            (expr, rest)
        } else if(!s.isEmpty && s.charAt(0).isDigit) {
            val (i, rest) = integer(s)
            (IntValue(i), rest)
        } else {
            val (v, rest) = variable(s)
            (v, rest)
        }
    }

    def identifier(s: String): (String, String) = {
        val (l, rest) = letter(s)
        val (d, rest1) = tryDigit(rest)
        d match {
            case Some(c) => (List(l,  c).mkString, rest1)
            case None    => (l.toString, rest)
        }
    }

    def variable(s: String): (Variable, String) = {
        val (name, s1) = identifier(s)
        val (sub, rest) = trySubscript(s1)
        (Variable(name, sub), rest)
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

    def stmGosub(s: String): (Gosub, String) = {
        val s1 = require(GOSUB, s)
        val (nr, rest) = integer(s1)
        (Gosub(nr), rest)
    }

    def stmReturn(s: String): (Return, String) = {
        val rest = require(RETURN, s)
        (Return(), rest)
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
