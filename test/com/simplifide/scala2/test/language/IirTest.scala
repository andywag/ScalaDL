package com.simplifide.scala2.test.language

import com.simplifide.generate.generator.CodeWriter
import com.simplifide.generate.signal._
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.parser.model.{SignalType, Expression}
import com.simplifide.generate.hier2.Entity
import com.simplifide.generate.TestConstants
import com.simplifide.generate.project2.{Project, Module}
import com.simplifide.generate.signalproc.Filter
import com.simplifide.generate.signal.Constant._
import com.simplifide.generate.test.Test._
import com.simplifide.generate.test.{Test, Isim, TestModule}

/**
 * This test case is a simple example of an iir filter. It contains all of the classes required to build and test
 * this module. The code containing the actual operation of a fully parameterizable filter is only about 10 lines of
 * code.
 *
 * The design files can be found at       XXXX/iir/design/XXXX
 * The test files can be found at         XXXX/iir/test/XXXX
 * The html documentation can be found at XXXX/iir/doc/XXXX
 *
 */

class IirTest {

}

object IirTest {

  /** Project which contains a simple IIR */
  object IirProject extends Project {
    // Location where the project is created
    val location:String = TestConstants.locationPrefix + "outputs" + TestConstants.separator + "iir"

    // Clock used for the design
    implicit val clk         = ClockControl("clk","reset")
    // Number of Filter Taps
    val len = 3
    // Array of Signals used for the numerator taps
    val num             = array("num",INPUT,S(8,6))(len)
    // Array of Signals used for the denominator taps
    val den             = array("den",INPUT,S(8,6))(len)
    // Input Output Signals
    val x              = signal("signal_in1",INPUT,S(8,6))
    val z              = signal("signal_out",OUTPUT,S(8,6))
    // Internal Width
    val iW = S(12,8)
    // Top Level Module for the Design
    override val root = new IirEntity("iir",num,den,x,z,iW)
    // List of Test Cases
    override val tests    = List(Test(new TestCase(root)))
    // Type of Test -- For this case ISIM
    override val testType = Some(new Isim(this))
  }

  /** Entity for the IIR */
  class IirEntity(name:String,
          val a:ArrayTrait[SignalTrait],
          val b:ArrayTrait[SignalTrait],
          val x:SignalTrait,
          val z:SignalTrait,
          val iW:FixedType)(implicit clk:ClockControl) extends Entity.Root(name,name)(clk) {
    // Input Output Signals
    override val signals = clk.allSignals(INPUT) ::: List(a,b,x,z)
    // Function which creates a module
    override val createModule = new IIR(this,iW).createModule
  }

  /** Generic IIR Implementation */
  class IIR(entity:IirEntity,
          val iW:FixedType)(implicit clk:ClockControl) extends Module(entity.name) {

    // Import the signals from the entity
    import entity._
    val n = clk                   // Assign the clk to n for convenience
    val len = a.numberOfChildren  // Number of Taps for the filter
    // Internal Signal Declarations
    val internal     = signal("internal_sig",WIRE,iW)
    val internal_out = signal("internal_out",WIRE,z.fixed)
    val y            = register(internal)(len-1)
    // Feedback Taps
    y(n) := x(n)    + List.tabulate(len)(i => RC(a(i)*y(n-i-1))).reduceLeft[Expression](_+_)
    // Feed-forward Taps
    internal_out(n) := List.tabulate(len)(i => RC(b(i)*y(n-i-1))).reduceLeft[Expression](_+_)
    // Output Assignment
    z := internal_out

  }
  /** Test Case for the Filter */
  class TestCase(val entity:IirEntity)(implicit clk:ClockControl) extends TestModule("test_iir",entity) {
    // Tap Values for numerator and denominator
    val num = List(.3,.58,.3)
    val den = List(0.0,.17,0.0)
    // Input values currently a sine wave
    val inputs:List[Double] = List.tabulate(length + 3)(x => math.sin(x.toDouble/64.0))
    val outputs = inputs.map(x => Constant(x,entity.z.fixed))
    // Test Assignment to the numerator and the denominator
    num.zipWithIndex.foreach(x => entity.a(x._2) --> x._1)
    den.zipWithIndex.foreach(x => entity.b(x._2) --> x._1)
    entity.x           --> (inputs.map(Constant(_)))        // Assign the System Input
    entity.z           <-- (outputs,3,length+3,4)           // Check the System Output

    this.createTest
  }

  def createProject = IirProject.createProject2

  def main(args:Array[String]) = {
    IirProject.createProject2
  }
}