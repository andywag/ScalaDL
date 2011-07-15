package com.simplifide.scala2.test.block

import com.simplifide.generate.blocks.basic.flop.FlopControl
import com.simplifide.generate.generator.CodeWriter
import com.simplifide.generate.signal.complex.{ComplexConstant, ComplexSignal}
import com.simplifide.generate.blocks.basic.fixed.{ComplexCSDMultiply, AdderTree}
import com.simplifide.generate.signal.{Constant, SignalTrait, ArrayTrait2, FixedType}

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 7/1/11
 * Time: 7:12 PM
 * To change this template use File | Settings | File Templates.
 */

class AdderTreeTest {

}

object AdderTreeTest {

  def main(args:Array[String]) = {
     val output          = SignalTrait("adder_tree_output",FixedType.signed(8,6))
     val inputPrototype  = SignalTrait("adder_tree_input",FixedType.signed(8,6))
     val inputs          = ArrayTrait2.newArray(3,inputPrototype)
     val constant1       = Constant.newConstant(.5,FixedType.signed(8,6))
     val constant2       = Constant.newConstant(.25,FixedType.signed(8,6))

     val tree = new AdderTree("adder_tree",
       FlopControl.default,
       output,
       List(new AdderTree.Value(constant2,inputs.slice(0)),
            new AdderTree.Value(constant1,inputs.slice(1)),
            new AdderTree.Value(constant2,inputs.slice(2))),
       FixedType.signed(12,8))

     System.out.println(tree.createCode(CodeWriter.Verilog).code)

  }

}

object ComplexTreeTest {
     def main(args:Array[String]) = {
     val output          = ComplexSignal.newComplex("adder_tree_output",FixedType.signed(8,6))
     val input           = ComplexSignal.newComplex("adder_tree_output",FixedType.signed(8,6))
     val constant        = ComplexConstant.newComplex(FixedType.signed(8,6),.5,.25)

     val tree = new ComplexCSDMultiply("complex_multiply",
       FlopControl.default,
       constant,
       input,
       output,
       FixedType.signed(12,8),
       2)

       System.out.println(tree.createCode(CodeWriter.Verilog).code)

  }


}