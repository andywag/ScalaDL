package com.simplifide.generate.language

import com.simplifide.generate.blocks.basic.flop.SimpleFlopList
import com.simplifide.generate.parser.model.{Expression, Clock}
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.generator.SimpleSegment
import com.simplifide.generate.blocks.basic.fixed.MultiplySegment
import com.simplifide.generate.signal.complex.ComplexSignal
import com.simplifide.generate.blocks.basic.fixed.complex.ComplexMultiplySegment

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 8/8/11
 * Time: 5:25 PM
 * To change this template use File | Settings | File Templates.
 */

class FlopFactory {

}

object FlopFactory {
    def apply(clk:Clock,output:Expression,input:Expression):SimpleSegment =  {
      input match {
        case MultiplySegment.Truncate(name,a:ComplexSignal,b:ComplexSignal,fixed,internal) =>
          new ComplexMultiplySegment.Truncate(output.name,clk,output.asInstanceOf[ComplexSignal],a,b,internal)
        case MultiplySegment.TruncateClip(name,a:ComplexSignal,b:ComplexSignal,fixed,internal) =>
          new ComplexMultiplySegment.TruncateClip(output.name,clk,output.asInstanceOf[ComplexSignal],a,b,internal)
        case MultiplySegment.Round(name,a:ComplexSignal,b:ComplexSignal,fixed,internal) =>
          new ComplexMultiplySegment.Round(output.name,clk,output.asInstanceOf[ComplexSignal],a,b,internal)
        case MultiplySegment.RoundClip(name,a:ComplexSignal,b:ComplexSignal,fixed,internal) =>
          new ComplexMultiplySegment.RoundClip(output.name,clk,output.asInstanceOf[ComplexSignal],a,b,internal)
        /*case MultiplySegment(name,a:ComplexSignal,b:ComplexSignal,fixed,internal) =>
          new ComplexMultiplySegment(output.name,clk,output.asInstanceOf[ComplexSignal],a,b,internal) */
        case _ => simpleFlop(clk,output,input)
      }
    }

    def simpleFlop(clk:Clock,output:Expression,input:Expression) = {
      val res = List(new SimpleFlopList.Segment(output,None))
      val en  = List(new SimpleFlopList.Segment(output,Some(input)))
      new SimpleFlopList(None,clk,res,en)
    }
}