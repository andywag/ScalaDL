package com.simplifide.scala2.test.language

import com.simplifide.generate.signal._
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.parser.model.{SignalType, Expression}
import math._
import com.simplifide.generate.parameter.{Parameter, ModuleScope}
import com.simplifide.generate.TestConstants
import com.simplifide.generate.hier2.Entity
import com.simplifide.generate.project2.{Project, Module}
import com.simplifide.generate.test.Test._
import com.simplifide.generate.test.{Test, Isim, TestModule}
import com.simplifide.generate.generator.{SimpleSegment, CodeWriter}
import com.simplifide.generate.signalproc.Filter

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 7/12/11
 * Time: 8:44 AM
 * To change this template use File | Settings | File Templates.
 */

class FirTest {

}


object FirTest {


  class FirEntity(name:String,
                  val input:SignalTrait,
                  val output:SignalTrait,
                  val taps:ArrayTrait[SignalTrait],
                  val iW:FixedType)(implicit clk:ClockControl) extends Entity.Root(name,name)(clk) {

    override val signals =  clk.allSignals(INPUT) ::: List(input,output,taps)
    override val createModule = new Fir(this,iW).createModule

  }


   /** Generic Fir Impl */
class Fir(val entity:FirEntity,
          val iW:FixedType)(implicit val clk:ClockControl) extends Module(entity.name) {

  import entity._

  description = Some(<p>This module is a generic FIR filter ...</p>)

  val length    = taps.length    // Number of Taps - Length of Filter
  val logLength = ceil(log(length)/log(2.0)).toInt + 1

  val rowLengths = List.tabulate(logLength)(i => math.ceil(length/pow(2.0,i)).toInt) // Length of Adder Row In Tree

  this.assignClock(clk)  // Add the clock modules

  signal(taps)   --: <p>Input <i>Taps</i></p>
  signal(input)
  signal(output)

  // Create the Signals for the module
  val delayLine       = register(input)(taps.length)             --: "Registers for the Delay Line with the Delay Line as Well"
  val multiplierOut   = array("mult_out",WIRE,iW)(taps.length)   --: "Output of the Multiplier"
  val adderRow   = List.tabulate(logLength)(i => array("mult_row_" + i,WIRE,iW)(rowLengths(i)) --: ("Adder Row " + i)) // List of Rows inside the adder tree

  /- ("Initial Tap Multiplication")
  multiplierOut := RC(delayLine * taps)      // Initial Tap Multiplication Section
  // Adder Tree
  for (i <- 1 until logLength) {
    /- ("Adder Tree Stage " + (i-1))
    val treeInput = if (i == 1) multiplierOut else adderRow(i-1)
    for (j <- 0 until rowLengths(i)) {
      if (2*j+1 >= adderRow(i-1).length) adderRow(i)(j) := treeInput(2*j)
      else                               adderRow(i)(j) := RC(treeInput(2*j) + treeInput(2*j+1))
    }
  }
  /- ("Output Stage")
  output := RC(adderRow(logLength-1)(0))

  }

  class TestCase(val entity:FirEntity)(implicit clk:ClockControl) extends TestModule("test_cordic",entity) {

    val taps = List(-.5,1.0,-.5,.25)
    val inputs:List[Double] = List.tabulate(length + 3)(x => math.sin(x.toDouble/64.0))
    val outputs = Filter.fir(taps,inputs).map(x => Constant(x,entity.output.fixed))

    taps.zipWithIndex.foreach(x => entity.taps(x._2) --> x._1)  // Assign the Taps for the Module

    entity.input           --> (inputs.map(Constant(_)))        // Assign the System Input
    entity.output          <-- (outputs,3,length+3,4)           // Check the System Output


     this.createTest

  }

  object Util {
    def filter(taps:List[Double],inputs:List[Double]) {

    }
  }

  object FirProject extends Project {
    val location:String = TestConstants.locationPrefix + "language" + TestConstants.separator + "fir_output"

    implicit val clk         = ClockControl("clk","reset")

    val len = 4

    val x              = signal("signal_in1",INPUT,S(8,6))
    val z              = signal("signal_out",OUTPUT,S(8,6))
    val taps           = array("taps",INPUT,S(8,6))(len)
    val iW = S(12,8)
    val iir = new FirEntity("fir",x,z,taps,iW)

    override val root = iir
    override val tests    = List(Test(new TestCase(root)))
    override val testType = Some(new Isim(this))



}

  def main(args:Array[String]) = {
    FirProject.createProject2
  }

}




