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
            test("PRINT 1", Program(Seq(Line(Print(IntValue(1)))))),
            test("PRINT -1", Program(Seq(Line(Print(Neg(IntValue(1))))))),
            test("PRINT X", Program(Seq(Line(Print(Variable("X")))))),
            test("PRINT -X", Program(Seq(Line(Print(Neg(Variable("X"))))))),
            test("PRINT X0", Program(Seq(Line(Print(Variable("X0")))))),
            test("PRINT X0+1", Program(Seq(Line(Print(Add(Variable("X0"),IntValue(1))))))),
            test("LET X=0", Program(Seq(Line(Let("X",IntValue(0)))))),
            test("LET X=X+1", Program(Seq(Line(Let("X",Add(Variable("X"),IntValue(1))))))),
            test("IF 1=2 THEN 99", Program(Seq(Line(If(Condition(IntValue(1),EQ(),IntValue(2)),99))))),
            test("IF 1<2 THEN 99", Program(Seq(Line(If(Condition(IntValue(1),LT(),IntValue(2)),99))))),
            test("IF 1>2 THEN 99", Program(Seq(Line(If(Condition(IntValue(1),GT(),IntValue(2)),99))))),
            test("IF 1<=2 THEN 99", Program(Seq(Line(If(Condition(IntValue(1),LE(),IntValue(2)),99))))),
            test("IF 1>=2 THEN 99", Program(Seq(Line(If(Condition(IntValue(1),GE(),IntValue(2)),99))))),
            test("IF 1<>2 THEN 99", Program(Seq(Line(If(Condition(IntValue(1),NE(),IntValue(2)),99)))))
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
