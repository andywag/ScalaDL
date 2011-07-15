package com.simplifide.generate.parser

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 6/23/11
 * Time: 11:29 AM
 * To change this template use File | Settings | File Templates.
 */

import SignalParser._

class SignalParserTest  {

}

object Tester extends SignalParser {

  /*def main(args:Array[String]) = {
    val n = new Model.Clock("clk")
    val x = this signal "x"
    val y = new Model.Sig("y")

    this assign  y(n) <= x(n) + y(n-1)


    this debug
  } */

}

object SignalParserTest extends SignalParser{

  val inFixed  = new Model.Fixed(8,6)
  val outFixed = new Model.Fixed(8,6)
  // Signal Declarations
  val x = new Signal("x",this)
  val y = new Signal("y",this)
  val z = new Signal("z",this)

  val a0 = new Signal("a0",this)
  val a1 = new Signal("a1",this)
  val b0 = new Signal("b0",this)
  val b1 = new Signal("b1",this)
  val b2 = new Signal("b2",this)

  def main(args:Array[String]) = {

      // Clock Generation
      val n = new Model.Clock("n")
      // Question Segment Test
      z <= x ? a0 :: b0
      // Conditional Test
      v_if (x) (
        v_if (y) {
          x := z
        }
      )
      v_else_if (x) (
        x := y(n-1)
      )
      v_else (
        x := y
      )
      // Simple Addition
      z <= x + (y + z)
      // IIR Block
      y(n) <= R(x(n) + a0*y(n-2) + a1*y(n-3),inFixed)
      z(n) <= b0*y(n) + b1*y(n-1) + b2*y(n-2)


      this.debug
  }
}


