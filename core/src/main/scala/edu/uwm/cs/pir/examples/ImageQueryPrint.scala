package edu.uwm.cs.pir.examples

import edu.uwm.cs.pir.pipeline._
import edu.uwm.cs.pir.domain._

object TestPrint extends Print with StringFunction with Example2 {
  def main(args: Array[String]): Unit = {
    imageQuery(PrintVisitor)
  }
}