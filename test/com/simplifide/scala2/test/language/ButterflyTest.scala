package com.simplifide.scala2.test.language

import com.simplifide.generate.language.Module
import com.simplifide.generate.generator.CodeWriter
import com.simplifide.generate.signal._
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.parser.model.{SignalType, Expression}
import com.simplifide.generate.blocks.basic.misc.Comment
import complex.ComplexSignal

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 7/12/11
 * Time: 8:44 AM
 * To change this template use File | Settings | File Templates.
 */

class ButterflyTest {

}

object ButterflyTest {


  object TestCase extends Module("alpha") {

     implicit val n = assignClock(ClockControl("clk","reset"))

     val fftSize    = 7
     val fftElement = (0,0)

     val twiddle = complex(math.sin(math.Pi/4),math.sin(math.Pi/4),S(8,6))

     val iw            = S(12,8)

     val sig1               = complex("signal_in1",INPUT,S(8,6))
     val sig2               = complex("signal_in2",INPUT,S(8,6))
     val sig_out            = complex("signal_out",OUTPUT,S(8,6))

     val sig1R              = register(sig1)(2)
     val multiplier_out     = complex_reg("mult_out",WIRE,iw)(1)
     val adder_out          = complex_reg("adder_out",WIRE,iw)(1)


     comment("Twiddle Factor Multiplication")
     multiplier_out(n)   := RC(twiddle * sig2)
     comment("Butterfly Adder")
     adder_out(n)        := RC(multiplier_out(n-1) + sig1(n-2))
     comment("Signal Assignment")
     sig_out             := adder_out(n-1)


  }

  class Butterfly(name:String,
    val clk:ClockControl,
    val sig_in1:ComplexSignal,
    val sig_in2:ComplexSignal,
    val sig_out1:ComplexSignal,
    val sig_out2:ComplexSignal,
    val twiddle:ComplexSignal,
    val iw:FixedType) extends Module(name) {

    implicit val n = clk

    this.signal(clk.getBus(INPUT),sig_in1,sig_in2,sig_out1.sig_out2,twiddle)

    val sig1R              = register(sig_in1)(2)
    val multiplier_out     = complex_reg("mult_out",WIRE,iw)(1)
    val adder_out          = complex_reg("adder_out",WIRE,iw)(1)


    comment("Twiddle Factor Multiplication")
    multiplier_out(n)   := RC(twiddle * sig_in1)
    comment("Butterfly Adder")
    adder_out(n)        := RC(multiplier_out(n-1) + sig_in2(n-2))
    comment("Signal Assignment")
    sig_out             := adder_out(n-1)

  }





  def main(args:Array[String]) = {

     val mod = TestCase.createModule
     System.out.println(mod.createCode(CodeWriter.Verilog))



  }
}