/*
  Author: uberset
  Date: 2015-11-14
  Licence: GPL v2
*/

package uberset.basic_compiler.backend


import java.io.PrintWriter
import uberset.basic_compiler.Parser


object TestGenerator86 {

    def main(args: Array[String]): Unit = {
        println(this.getClass.getSimpleName)
        val results = Seq(
            test("empty.asm", ""),
            test("hello.asm", "PRINT \"Hello World!\""),
            test("hello2.asm", "10 PRINT \"Hello\"\n20 PRINT \"World!\""),
            test("print1.asm", "PRINT 1"),
            test("print2.asm", "PRINT 32767"),
            test("add.asm", "PRINT \"5+2=\"\nPRINT 5+2\n"),
            test("sub.asm", "PRINT \"5-2=\"\nPRINT 5-2\n"),
            test("mul.asm", "PRINT \"5*2=\"\nPRINT 5*2\n"),
            test("div.asm", "PRINT \"5/2=\"\nPRINT 5/2\n"),
            test("neg.asm", "PRINT \"-5=\"\nPRINT -5\n"),
            test("var.asm", "PRINT X"),
            test("sdiv.asm", "LET X=-1\nPRINT \"-1/-1=\"\nPRINT X/X\n"),
            test("goto1.asm", "10 PRINT \"Hello\"\n20 GO TO 10"),
            test("if1.asm", "LET I=1\n10 PRINT I*I\nLET I=I+1\nIF I<=5 THEN 10\n"),
            test("rem1.asm", "REM do nothing\n"),
            test("input.asm", "INPUT X\nPRINT X+1"),
            test("expression.asm", "PRINT \"-3+4*(5+6)*7+8-9=\"\nPRINT -3+4*(5+6)*7+8-9"),
            test("gosubre.asm",
                 """GOSUB 100
                   |GOSUB 100
                   |GOSUB 100
                   |GOTO 999
                   |100 PRINT "Hello!"
                   |RETURN
                   |999 REM END
                   |""".stripMargin),
            test("array.asm",
                 """DIM A(32463)
                   |LET A(32463) = -1
                   |INPUT A(0)
                   |PRINT A(32463)
                   |PRINT A(0)
                   |PRINT A(1)
                   |""".stripMargin),
            test("fornext.asm",
                 """FOR I=1 TO 9 STEP 2
                   |  PRINT I*I
                   |NEXT I
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

    def test(outfilename: String, text: String): Boolean = {
        try {
            val prog = Parser.parse(text)
            val outStr = Generator86.generate(prog).mkString
            new PrintWriter("output/backend/generator86/"+outfilename) { write(outStr); close }
            true
        } catch {
            case e: Exception =>
                println(e.toString)
                e.printStackTrace()
                false
        }
    }

}
