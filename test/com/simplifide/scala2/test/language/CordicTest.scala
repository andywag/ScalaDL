package com.simplifide.scala2.test.language

import com.simplifide.generate.language.Module
import com.simplifide.generate.generator.CodeWriter
import com.simplifide.generate.signal._
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.parser.model.{SignalType, Expression}
import com.simplifide.generate.blocks.basic.misc.Comment

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 7/12/11
 * Time: 8:44 AM
 * To change this template use File | Settings | File Templates.
 */

class CordicTest {

}

object CordicTest {


  object TestCase extends Module("alpha") {

     implicit val n = assignClock(ClockControl("clk","reset"))


     val stages = 8

     val iW = S(12,8)
     val iaW = S(12,8)

     val signal              = complex("signal_in",INPUT,S(8,6))
     val angle               = signal ("angle_in",INPUT,S(8,8))

     val signal_out          = complex("signal_out",OUTPUT,S(8,6))

     val signalI     = register(complex_array("signal_internal",WIRE,iW)(stages))(1)
     val angleI      = register(array("angle_internal",WIRE,iW)(stages))(1)

     for (i <- 1 until stages) {
        val sh = math.pow(2.0,-i)
        comment("Cordic Stage" + i)
        comment("Real Calculation")
        signalI(n)(i).real := (signalI(n-1)(i).imag)      ? (signalI(n-1)(i-1).real + sh*signalI(n-1)(i-1).imag) :: (signalI(n-1)(i-1).real - sh*signalI(n-1)(i-1).imag)
        comment("Imaginary Calculation")
        signalI(n)(i).imag := (signalI(n-1)(i).imag)      ? (signalI(n-1)(i-1).imag - sh*signalI(n-1)(i-1).real) :: (signalI(n-1)(i-1).real + sh*signalI(n-1)(i-1).imag)
        comment("Angle Calculation")
        angleI(n)(i)       := (signalI(n-1)(i).imag.sign) ?  (angleI(n-1)(i-1) + math.atan2(1.0,sh)) :: (angleI(i-1) - math.atan2(1.0,sh))

     }

  }





  def main(args:Array[String]) = {

     val mod = TestCase.createModule
     System.out.println(mod.createCode(CodeWriter.Verilog))



  }
}