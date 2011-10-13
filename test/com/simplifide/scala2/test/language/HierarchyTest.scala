package com.simplifide.scala2.test.language

import com.simplifide.generate.project2.{Module, Project}
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.signal.complex.ComplexSignal
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.parameter.{ModuleScope, Parameter}
import com.simplifide.generate.hier2.Entity
import com.simplifide.scala2.test.language.HierarchyTest.RootA
import com.simplifide.generate.signal._
import com.simplifide.generate.parser.model.SignalType
import com.simplifide.generate.TestConstants
import com.simplifide.generate.test.Test._
import com.simplifide.generate.test.{Test, TestModule, Isim}

/**
 * This test case gives an example of how the hierarchy is designed. Module connections are automatically handled
 * by name with a method to override the naming convention. The approach is similar to emacs autoargs
 *
 * The design files can be found at       XXXX/hier/design/XXXX
 * The test files can be found at         XXXX/hier/test/XXXX
 * The html documentation can be found at XXXX/hier/doc/XXXX
 *
 */


/** FFT Project class which contains the list of modules and file locations */
class HierarchyTest extends Project {
  // Clock which is used for the design.
  implicit val clk = ClockControl("clk","reset")
  // Project Location of the design
  val location:String = TestConstants.locationPrefix + "outputs" + TestConstants.separator + "hier"
  // Top Level Module for the Design
  override val root     = new RootA()
  // Defines the Tests for this project
  override val tests    = List(Test(new HierarchyTest.TestCase(root)))
  // Selects the simulator for this module - ISIM for this case
  override val testType = Some(new Isim(this))
}

object HierarchyTest {
  // Clock which is used for the design.
  implicit val clk = ClockControl("clk","reset")

  /** Root Module of the Design */
  class RootA extends Entity.Root("rootA","rootA") {
    // Definition of an instance for this module
    val branchA = new BranchA()
    // List of Instances in this module
    override val entities = List(branchA)
  }

  /** Sub-block of the Root Module */
  class BranchA extends Entity.Branch("branchA","branchA") {
    // Creation of Leaf Modules
    val leafA = new LeafA(this)
    val leafB = new LeafB(this)
    // List of Instances in the design
    override val entities = List(leafA,leafB)
  }

  // Example Leaf Module
  class LeafA(val parent:BranchA)(implicit clk:ClockControl) extends Entity.Leaf("leafA","leafA") {
    // Input and Output Signals Defined
    val modInput  = Bus("a_in",TestBus)
    val modOutput = Bus("b_in",TestBus.reverseType) // Reverse Type reverses the direction of this output
    // List of Signals in this entity
    override val signals = clk.allSignals(INPUT) ::: List(modInput,modOutput)
  }

  // Example Leaf Module
  class LeafB(val parent:BranchA)(implicit clk:ClockControl) extends Entity.Leaf("leafB","leafB") {
    // Output of this Module
    val modOutput = Bus("b_out",TestBus.reverseType)
    // List of Signals in the Design. The output of LeafA is included which will be automatically connected
    override val signals = clk.allSignals(INPUT) ::: List(parent.leafA.modOutput.reverseType,modOutput)
  }

      /** Test Case for the State Machine */
  class TestCase(val entity:RootA)(implicit clk:ClockControl) extends TestModule("state_test",entity) {
    // Run the condition which controls the state machine like a counter
    this.createTest
  }



  /** Bus which is used in this design for signal connections */
  object TestBus extends BusType {
    override val signals:List[SignalTrait] =
        List(SignalTrait("rdy",OpType.Output),
             SignalTrait("data",OpType.Input),
             SignalTrait("vld",OpType.Input))
  }


  def main(args:Array[String]) = {
    val test = new HierarchyTest()
    test.createProject2
  }




}

