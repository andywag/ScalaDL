package com.simplifide.scala2.test.language

import com.simplifide.generate.language.Module
import com.simplifide.generate.generator.CodeWriter
import com.simplifide.generate.signal._
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.parser.model.{SignalType, Expression}

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 7/12/11
 * Time: 8:44 AM
 * To change this template use File | Settings | File Templates.
 */

class SignalProcessingTest {

}

object SignalProcessingTest {


  object TestCase extends Module("alpha") {

     val clk         = ClockControl("clk","reset")
     val n = clk
     val clk_signal  = signal(clk.getBus(OpType.Input))

     val len = 5

     val a             = array("num",INPUT,S(8,6))(len)
     val b             = array("den",INPUT,S(8,6))(len)

     val x              = signal("signal_in1",INPUT,S(8,6))
     val z              = signal("signal_out",OUTPUT,S(8,7))

     val y              = register(signal("internal",WIRE,S(8,6)),n)(len)

      val iW = S(12,8)

     // Simple Example - No Rounding/Clipping

     //y(n) := x(n)    + a(0)*y(n-1) + a(1)*y(n-2)
     //z(n) := b*y(n)  + b(1)*y(n-1) + b(2)*y(n-2)

     // Round and clip the inputs multiplier outputs and the adder

     //y(n) := RC(x(n)    + RC(a(0)*y(n-1),iW) + RC(a(1)*y(n-2),iW))
     //z(n) := RC(RC(b*y(n),iW)  + RC(b(1)*y(n-1),iW) + RC(b(2)*y(n-2),iW))

     // Generalize the filter to any length -- Kind of architecturally this design will have timing issues
     //y(n) := x(n) + List.tabulate(len)(i => RC(a(i)*y(n-i))).reduceLeft[Expression](_+_)
     //z(n) := List.tabulate(len)(i => RC(b(i)*y(n-i))).reduceLeft[Expression](_+_)

     val con = constant(.5)
     z := con * x

  }

  /** Generic IIR Module */
  class IIR(name:String, val n:ClockControl,val a:SignalTrait,
              val b:SignalTrait, val x:SignalTrait, val z:SignalTrait,
              val iW:FixedType) extends Module(name) {

      signal(n.getBus(OpType.Input), a, b, x, z) // Add the clock modules

     val len = a.numberOfChildren

     val internal     = signal("internal",WIRE,iW)
     val y = register(internal,n)(len-1)

     y(n) := x(n)    + List.tabulate(len)(i => RC(a(i)*y(n-i))).reduceLeft[Expression](_+_)
     z(n) := List.tabulate(len)(i => RC(b(i)*y(n-i))).reduceLeft[Expression](_+_)

  }


  def main(args:Array[String]) = {

     val mod = TestCase.createModule
     System.out.println(mod.createCode(CodeWriter.Verilog))



  }
}