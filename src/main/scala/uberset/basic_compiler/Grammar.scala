/*
  Author: uberset
  Date: 2015-11-14
  Licence: GPL v2
*/

package uberset.basic_compiler

// string = '"' string-char* '"' ;
// string-char = ? any character except '"' and newline> ? ;
// comment-text = comment-char* ;
// comment-char = ? any character except newline> ? ;
// integer = digit+
// digit = '0' ... '9'
// letter = 'A' ... 'Z'

// program = line* ;
case class Program(lines: Seq[Line])

// line = [integer] statement ;
case class Line(nr: Option[Int], stm: Statement)
// statement = print | goto | let | if | rem | input ;
sealed abstract class Statement

// input = 'INPUT' variable ;
case class Input(variable: String) extends Statement

// rem = 'REM' comment-text ;
case class Rem() extends Statement

// if = 'IF' condition 'THEN' integer ;
case class If(cond: Condition, line: Int) extends Statement

// condition = expression relop expression ;
case class Condition(expr1: Expression, op: RelOp, expr2: Expression)

// relop = ( '=' | '<' | '>' | '<=' | '>=' | '<>' ) ,
sealed abstract class RelOp
case class EQ() extends RelOp
case class LT() extends RelOp
case class GT() extends RelOp
case class LE() extends RelOp
case class GE() extends RelOp
case class NE() extends RelOp

// let = 'LET' variable '=' expression ;
case class Let(variable: String, expression: Expression) extends Statement

// print = 'PRINT' printargument ;
case class Print(arg: PrintArgument) extends Statement

// printargument = string | expression ;
trait PrintArgument
case class StringArg(string: String) extends PrintArgument

// expression = ['-'|'+'] term (addoperation term)* ;
case class Expression(negation: Boolean, term: Term, ops: List[(AddOp, Term)]) extends Factor with PrintArgument

// term = factor (muloperation factor)* ;
case class Term(factor: Factor, ops: List[(MulOp, Factor)])

// factor = intvalue | variable | "(" expression ")" ;
sealed abstract class Factor

// intvalue = integer ;
case class IntValue(value: Int) extends Factor

// variable = letter digit? ;
case class Variable(name: String) extends Factor

// addoperation = ( '+' | '-'  ) ;
sealed abstract class AddOp
case class Add() extends AddOp
case class Sub() extends AddOp

// muloperation = ( '*' | '/' ) ;
sealed abstract class MulOp
case class Mul() extends MulOp
case class Div() extends MulOp

// goto = 'GOTO' integer ;
case class Goto(nr: Int) extends Statement


object Line {
    def apply(stm: Statement): Line = Line(None, stm)
    def apply(nr: Int, stm: Statement): Line = Line(Some(nr), stm)
}

object Print {
    def apply(string: String): Print = Print(StringArg(string))
}

object Expression {
    def apply(term: Term, list: List[(AddOp, Term)]): Expression = Expression(true, term, list)
    def apply(term: Term): Expression = Expression(false, term, List())
    def apply(neg: Boolean, term: Term): Expression = Expression(neg, term, List())
    def apply(t1: Term, op: AddOp, t2: Term): Expression = Expression(false, t1, List((op, t2)))
    def apply(neg: Boolean, t1: Term, op: AddOp, t2: Term): Expression = Expression(neg, t1, List((op, t2)))
}

object Term {
    def apply(factor: Factor): Term = Term(factor, List())
}
