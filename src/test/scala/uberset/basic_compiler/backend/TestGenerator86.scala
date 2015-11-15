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
            test("neg.asm", "PRINT \"-5=\"\nPRINT -5\n")
            //test("var.asm", "PRINT X"),
            //test("sdiv.asm", "LET X=-1\nPRINT \"-1/-1=\"\nPRINT X/X\n")
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
                println(e.getMessage)
                false
        }
    }

}
