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

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 8/12/11
 * Time: 2:10 PM
 * To change this template use File | Settings | File Templates.
 */


/** FFT Project class which contains the list of modules and file locations */
class HierarchyTest extends Project {

  val location:String = "C:\\designs2\\Generator\\test\\com\\simplifide\\scala2\\test\\language\\hier_output"

    override val root     = RootA
}

object HierarchyTest {

  implicit val clk = ClockControl("clk","reset")

  object TestBus extends BusType {
    override val signals:List[SignalTrait] =
        List(SignalTrait("rdy",OpType.Output),
             SignalTrait("data",OpType.Input),
             SignalTrait("vld",OpType.Input))

  }

  object RootA extends Entity.Root("rootA","rootA") {
    val branchA = new BranchA()

     override val entities = List(branchA)
  }

  class BranchA extends Entity.Branch("branchA","branchA") {
     val leafA = new LeafA(this)
     val leafB = new LeafB(this)

     override val entities = List(leafA,leafB)
  }

  class BranchB extends Entity.Branch("branchB","branchB") {

  }

  class LeafA(val parent:BranchA) extends Entity.Leaf("leafA","leafA") {

    val modInput  = Bus("a_in",TestBus)
    val modOutput = Bus("b_in",TestBus.reverseType)

    override val signals = List(modInput,modOutput)
  }

  class LeafB(val parent:BranchA) extends Entity.Leaf("leafB","leafB") {
    val modOutput = Bus("b_out",TestBus.reverseType)

    override val signals = List(parent.leafA.modOutput.reverseType,
                                modOutput)
  }


  def main(args:Array[String]) = {
    val test = new HierarchyTest()
    test.createProject2
  }




}

