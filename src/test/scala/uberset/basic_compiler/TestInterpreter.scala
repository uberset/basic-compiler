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
                 "Hello\nWorld!\n")
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
