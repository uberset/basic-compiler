package uberset.basic_compiler

/*
  Author: uberset
  Date: 2015-11-09
  Licence: GPL v2
*/

object TestAll {
    def main(args: Array[String]): Unit = {
        TestParser.main(args)
        TestInterpreter.main(args)
    }
}
