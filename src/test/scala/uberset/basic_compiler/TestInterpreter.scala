package uberset.basic_compiler

/*
  Author: uberset
  Date: 2015-11-09
  Licence: GPL v2
*/

object TestInterpreter {

    def main(args: Array[String]): Unit = {
        println(this.getClass.getSimpleName)
        val results = Seq(
            test("""PRINT"Hello World!"""", "Hello World!\n"),
            test("""PRINT "Hello World!"""", "Hello World!\n"),
            test(""" P R I N T "Hello World!" """, "Hello World!\n"),
            test("""PRINT "Hello"
                   |PRINT "World!"""".stripMargin,
                 "Hello\nWorld!\n"),
            test("""10 GO TO 30
                   |20 PRINT "SKIP ME 20"
                   |30 PRINT "OK 30"""".stripMargin,
                 "OK 30\n"),
            test("PRINT 5", "5\n"),
            test("PRINT -2", "-2\n"),
            test("PRINT 5+2", "7\n"),
            test("PRINT 5-2", "3\n"),
            test("PRINT 5*2", "10\n"),
            test("PRINT 5/2", "2\n"),
            test("PRINT X", "0\n"),
            test("LET X=1\nPRINT X+1", "2\n"),
            test("LET X=1\nPRINT X\nLET X=X+1\nPRINT X", "1\n2\n"),
            test("IF 1<2 THEN 10\nPRINT \"false\"\nGOTO 20\n10 PRINT \"true\"\n20LET X=X", "true\n"),
            test("IF 1>2 THEN 10\nPRINT \"false\"\nGOTO 20\n10 PRINT \"true\"\n20LET X=X", "false\n"),
            test("IF 1=2 THEN 10\nPRINT \"false\"\nGOTO 20\n10 PRINT \"true\"\n20LET X=X", "false\n"),
            test("IF 1<=2 THEN 10\nPRINT \"false\"\nGOTO 20\n10 PRINT \"true\"\n20LET X=X", "true\n"),
            test("IF 1>=2 THEN 10\nPRINT \"false\"\nGOTO 20\n10 PRINT \"true\"\n20LET X=X", "false\n"),
            test("IF 1<>2 THEN 10\nPRINT \"false\"\nGOTO 20\n10 PRINT \"true\"\n20LET X=X", "true\n"),
            test(""" 10 LET N = 7
                   | 20 PRINT "Fibonacci number of"
                   | 30 PRINT N
                   | 40 PRINT "is"
                   | 70 LET F = 1
                   | 80 IF N < 1 THEN 140
                   | 90   LET I = 1
                   |100   LET F = F * I
                   |110   LET I = I + 1
                   |120   IF I <= N THEN 100
                   |140 PRINT F""".stripMargin,
                 "Fibonacci number of\n7\nis\n5040\n")
        )
        val tests = results.size
        val passed = results.filter(identity).size
        val failed = tests - passed
        if(failed>0)
            println(s"$failed of $tests tests failed.")
        else
            println(s"All $tests tests passed.")
    }

    def test(text: String, output: String): Boolean = {
        try {
            val prog = Parser.parse(text)
            val out = Interpreter.run(prog).mkString
            assertEquals(out, output)
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
