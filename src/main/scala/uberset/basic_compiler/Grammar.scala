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
// integer = digit+ ;
// digit = '0' ... '9' ;
// letter = 'A' ... 'Z' ;
// identifier = letter digit? ;


// program = line* ;
case class Program(lines: Seq[Line])

// line = [integer] statement ;
case class Line(nr: Option[Int], stm: Statement)
// statement = print | goto | gosub | return | let | if | rem | input | dim | for | next ;
sealed abstract class Statement

// next = 'NEXT' ;
case class Next(id: String) extends Statement

// for = 'FOR' identifier '=' expression 'TO' expression ( 'STEP' expression ) ;
case class For(id: String, from: Expression, to: Expression, step: Option[Expression]) extends Statement

// dim = 'DIM' identifier '(' integer ')' ;
case class Dim(variable: String, upper: Int) extends Statement

// input = 'INPUT' variable ;
case class Input(variable: Variable) extends Statement

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
case class Let(variable: Variable, expression: Expression) extends Statement

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

// variable = identifier ( '(' expression ')' ) ;
case class Variable(name: String, subscript: Option[Expression]) extends Factor

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

// gosub = 'GOSUB' integer ;
case class Gosub(nr: Int) extends Statement

// return = 'RETURN' ;
case class Return() extends Statement


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

object Variable {
    def apply(name: String): Variable = Variable(name, None)
    def apply(name: String, expression: Expression): Variable = Variable(name, Some(expression))
}

object For {
    def apply(id: String, from: Expression, to: Expression): For = For(id, from, to, None)
    def apply(id: String, from: Expression, to: Expression, step: Expression): For = For(id, from, to, Some(step))
}
