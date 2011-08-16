package com.simplifide.generate.parser

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 6/23/11
 * Time: 11:29 AM
 * To change this template use File | Settings | File Templates.
 */

import model.{Clock, Signal, Model}


class SignalParserTest  {

}

object Tester extends SignalParser {

  /*def main(args:Array[String]) = {
    val n = new Model.Clock("clk")
    val x = this appendSignal "x"
    val y = new Model.Sig("y")

    this assign  y(n) <= x(n) + y(n-1)


    this debug
  } */

}

object SignalParserTest extends ModuleParser{

  val inFixed  = Model.Fixed(8,6)
  val outFixed = Model.Fixed(8,6)
  // Signal Declarations
  val x = Signal("x")
  val y = Signal("y")
  val z = Signal("z")

  val a0 = Signal("a0")
  val a1 = Signal("a1")
  val b0 = Signal("b0")
  val b1 = Signal("b1")
  val b2 = Signal("b2")

  def main(args:Array[String]) = {



      // Clock Generation
      val n = Clock("n")
      // Question Segment Test
      //z <= x ? a0 :: b0

      //y <= ~x
      //y <= $cat(x)
      // Case Test
      //$case(x) (
      //  x -> (x <= a0),
      //  y -> y
      //)

      // Conditional Test
      /*
      $if (x) (
        x <= a0,
        $if (y) (
          x <= b2
        ),
        $else (
          x <= a1
        )
      )
      $else_if (x) (
        x <= b0
      )
      $else (
        x <= b1
      )
       */
      // Simple Addition
      //z <= x + (y + z)
      // Simple Flop with No Reset
      //z <= (0 -> (x + y)) @@ n

      // IIR Block

      //x <= z - y

      x     := y

      x    := R(x,inFixed)
      // y(n) := x(n)    + a0*y(n-2) + a1*y(n-3)
      // z(n) := b0*y(n) - b1*y(n-1) + b2*y(n-2)

      this.transform
      this.debug
      //x := x & x | x

  }

  object StateMachineTest extends ModuleParser {
       def main(args:Array[String]) = {
        //val edge = new State("a",1) ~> new State("b",2)
       }
  }

}


