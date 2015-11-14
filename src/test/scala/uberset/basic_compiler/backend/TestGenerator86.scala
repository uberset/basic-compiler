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
            test("empty.asm", "")
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
