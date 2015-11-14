/*
  Author: uberset
  Date: 2015-11-09
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
// statement = print | goto | let | if | rem ;
sealed abstract class Statement

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


object Line {
    def apply(stm: Statement): Line = Line(None, stm)
    def apply(nr: Int, stm: Statement): Line = Line(Some(nr), stm)
}

object Print {
    def apply(string: String): Print = Print(StringArg(string))
}
