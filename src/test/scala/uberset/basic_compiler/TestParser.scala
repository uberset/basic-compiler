package uberset.basic_compiler

/*
  Author: uberset
  Date: 2015-11-09
  Licence: GPL v2

*/

object TestParser {

    def main(args: Array[String]): Unit = {
        val results = Seq(
            test("""PRINT"Hello World!"""", Program(Print("Hello World!"))),
            test("""PRINT "Hello World!"""", Program(Print("Hello World!"))),
            test(""" P R I N T "Hello World!" """, Program(Print("Hello World!")))
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
