/*
  Author: uberset
  Date: 2015-11-23
  Licence: GPL v2
*/

package uberset.basic_compiler.backend

import java.io.PrintWriter

import uberset.basic_compiler.Parser


object TestGeneratorJVM {

    def main(args: Array[String]): Unit = {
        println(this.getClass.getSimpleName)
        val results = Seq(
            test("empty", ""),
            test("hello", "PRINT \"Hello World!\""),
            test("hello2", "10 PRINT \"Hello\"\n20 PRINT \"World!\""),
            test("print1", "PRINT 1"),
            test("print2", "PRINT 32767"),
            test("add", "PRINT \"5+2=\"\nPRINT 5+2\n"),
            test("sub", "PRINT \"5-2=\"\nPRINT 5-2\n"),
            test("mul", "PRINT \"5*2=\"\nPRINT 5*2\n"),
            test("div", "PRINT \"5/2=\"\nPRINT 5/2\n"),
            test("neg", "PRINT \"-5=\"\nPRINT -5\n"),
            test("var", "PRINT X"),
            test("sdiv", "LET X=-1\nPRINT \"-1/-1=\"\nPRINT X/X\n"),
            test("goto1", "10 PRINT \"Hello\"\n20 GO TO 10"),
            test("if1", "LET I=1\n10 PRINT I*I\nLET I=I+1\nIF I<=5 THEN 10\n"),
            test("rem1", "REM do nothing\n"),
            test("input", "INPUT X\nPRINT X+1"),
            test("expression", "PRINT \"-3+4*(5+6)*7+8-9=\"\nPRINT -3+4*(5+6)*7+8-9"),
            test("gosubre",
                 """GOSUB 100
                   |GOSUB 100
                   |GOSUB 100
                   |GOTO 999
                   |100 PRINT "Hello!"
                   |RETURN
                   |999 REM END
                   |""".stripMargin),
            test("array",
                 """DIM A(32463)
                   |LET A(32463) = -1
                   |INPUT A(0)
                   |PRINT A(32463)
                   |PRINT A(0)
                   |PRINT A(1)
                   |""".stripMargin),
/* not implemented
            test("fornext",
                """FOR I=1 TO 9 STEP 2
                  |  PRINT I*I
                  |NEXT I
                  |""".stripMargin),
            test("for1",
                """FOR I=1 TO 5
                  |  PRINT I
                  |NEXT I
                  |""".stripMargin),
*/
            test("fornext",
                 """LET I=1
                   |10  PRINT I*I
                   |      LET I = I + 2
                   |IF I <= 9 THEN 10
                   |""".stripMargin),
            test("for1",
                 """LET I=1
                   |10  PRINT I
                   |      LET I = I + 1
                   |IF I <= 5 THEN 10
                   |""".stripMargin)
        )
        val tests = results.size
        val passed = results.filter(identity).size
        val failed = tests - passed
        if(failed>0)
            println(s"$failed of $tests tests failed.")
        else
            println(s"All $tests tests passed.")
    }

    def test(mainName: String, text: String): Boolean = {
        try {
            val prog = Parser.parse(text)
            val outStr = GeneratorJVM.generate(prog, mainName).mkString
            new PrintWriter("output/backend/generatorJVM/"+mainName+".j") { write(outStr); close }
            true
        } catch {
            case e: Exception =>
                println(e.toString)
                e.printStackTrace()
                false
        }
    }

}
