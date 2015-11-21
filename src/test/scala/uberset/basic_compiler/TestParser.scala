package uberset.basic_compiler

/*
  Author: uberset
  Date: 2015-11-09
  Licence: GPL v2
*/

object TestParser {

    def main(args: Array[String]): Unit = {
        println(this.getClass.getSimpleName)
        val results = Seq(
            test("""PRINT"Hello World!"""", Program(Seq(Line(Print("Hello World!"))))),
            test("""PRINT "Hello World!"""", Program(Seq(Line(Print("Hello World!"))))),
            test(""" P R I N T "Hello World!" """, Program(Seq(Line(Print("Hello World!"))))),
            test("""PRINT "Hello"
                   |PRINT "World!"""".stripMargin,
                 Program(Seq(Line(Print("Hello")),
                            Line(Print("World!"))))),
            test("""10PRINT"Hello World!"""", Program(Seq(Line(10, Print("Hello World!"))))),
            test("""10 PRINT "Hello"
                   |20 GO TO 10""".stripMargin,
                 Program(Seq(Line(10, Print("Hello")), Line(20, Goto(10))))),
            test("PRINT 1", Program(Seq(Line(Print(Expression(Term(IntValue(1)))))))),
            test("PRINT -1", Program(Seq(Line(Print(Expression(true,Term(IntValue(1)))))))),
            test("PRINT X", Program(Seq(Line(Print(Expression(Term(Variable("X")))))))),
            test("PRINT -X", Program(Seq(Line(Print(Expression(true,Term(Variable("X")))))))),
            test("PRINT X0", Program(Seq(Line(Print(Expression(Term(Variable("X0")))))))),
            test("PRINT X0+1", Program(Seq(Line(Print(Expression(Term(Variable("X0")),Add(),Term(IntValue(1)))))))),
            test("LET X=0", Program(Seq(Line(Let(Variable("X"),Expression(Term(IntValue(0)))))))),
            test("LET X=X+1", Program(Seq(Line(Let(Variable("X"),Expression(Term(Variable("X")),Add(),Term(IntValue(1)))))))),
            test("IF 1=2 THEN 99", Program(Seq(Line(If(Condition(Expression(Term(IntValue(1))),EQ(),Expression(Term(IntValue(2)))),99))))),
            test("IF 1<2 THEN 99", Program(Seq(Line(If(Condition(Expression(Term(IntValue(1))),LT(),Expression(Term(IntValue(2)))),99))))),
            test("IF 1>2 THEN 99", Program(Seq(Line(If(Condition(Expression(Term(IntValue(1))),GT(),Expression(Term(IntValue(2)))),99))))),
            test("IF 1<=2 THEN 99", Program(Seq(Line(If(Condition(Expression(Term(IntValue(1))),LE(),Expression(Term(IntValue(2)))),99))))),
            test("IF 1>=2 THEN 99", Program(Seq(Line(If(Condition(Expression(Term(IntValue(1))),GE(),Expression(Term(IntValue(2)))),99))))),
            test("IF 1<>2 THEN 99", Program(Seq(Line(If(Condition(Expression(Term(IntValue(1))),NE(),Expression(Term(IntValue(2)))),99))))),
            test("REM Don't ignore me. I'm important.", Program(Seq(Line(Rem())))),
            test("INPUT X", Program(Seq(Line(Input(Variable("X")))))),
            test("PRINT -3+4*(5+6)*7+8-9", Program(Seq(Line(Print(Expression(
                Term(IntValue(3)),List(
                    (Add(),Term(IntValue(4),List(
                        (Mul(),Expression(Term(IntValue(5)),Add(),Term(IntValue(6)))),
                        (Mul(),IntValue(7))
                    ))),
                    (Add(),Term(IntValue(8))), (Sub(),Term(IntValue(9)))
                ))))))),
            test("GOSUB 100", Program(Seq(Line(Gosub(100))))),
            test("RETURN", Program(Seq(Line(Return())))),
            test("""DIM A(32767)
                   |LET A(32767) = -1
                   |INPUT A(0)
                   |PRINT A(32767)
                   |PRINT A(0)
                   |""".stripMargin,
                Program(Seq(
                    Line(Dim("A",32767)),
                    Line(Let(Variable("A",Expression(Term(IntValue(32767)))),
                             Expression(true,Term(IntValue(1))))),
                    Line(Input(Variable("A",Expression(Term(IntValue(0)))))),
                    Line(Print(Expression(Term(Variable("A",Expression(Term(IntValue(32767)))))))),
                    Line(Print(Expression(Term(Variable("A",Expression(Term(IntValue(0))))))))
                )))
        )
        val tests = results.size
        val passed = results.filter(identity).size
        val failed = tests - passed
        if(failed>0)
            println(s"$failed of $tests tests failed.")
        else
            println(s"All $tests tests passed.")
    }

    def test(text: String, tree: Program): Boolean = {
        try {
            val prog = Parser.parse(text)
            assertEquals(prog, tree)
        } catch {
            case e: Exception =>
                println(e.getMessage)
                false
        }
    }

    def assertEquals(value: AnyRef, expected: AnyRef): Boolean = {
        val condition = value == expected
        if(!condition) {
            println(s"value: $value does not equals expected: $expected")
        }
        condition
    }
}
