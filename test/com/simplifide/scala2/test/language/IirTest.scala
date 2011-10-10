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
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 7/12/11
 * Time: 8:44 AM
 * To change this template use File | Settings | File Templates.
 */

class IirTest {

}

object IirTest {

  class IirEntity(name:String,
          val a:SignalTrait,
          val b:SignalTrait,
          val x:SignalTrait,
          val z:SignalTrait,
          val iW:FixedType)(implicit clk:ClockControl) extends Entity.Root(name,name)(clk) {

    override val signals = clk.allSignals(INPUT) ::: List(a,b,x,z)
    override val createModule = new IIR(this,iW).createModule

  }

  /** Generic IIR Impl */
  class IIR(entity:IirEntity,
          val iW:FixedType)(implicit clk:ClockControl) extends Module(entity.name) {

    import entity._
    val n = clk
    val len = a.numberOfChildren

    val internal     = signal("internal_sig",WIRE,iW)
    val internal_out = signal("internal_out",WIRE,z.fixed)
    val y            = register(internal)(len-1)

    y(n) := x(n)    + List.tabulate(len)(i => RC(a(i)*y(n-i-1))).reduceLeft[Expression](_+_)
    internal_out(n) := List.tabulate(len)(i => RC(b(i)*y(n-i-1))).reduceLeft[Expression](_+_)
    z := internal_out

  }

    class TestCase(val entity:IirEntity)(implicit clk:ClockControl) extends TestModule("test_iir",entity) {

    val num = List(.3,.58,.3)
    val den = List(0.0,.17,0.0)

    num.zipWithIndex.foreach(x => entity.a(x._2) --> x._1)
    den.zipWithIndex.foreach(x => entity.b(x._2) --> x._1)

    val inputs:List[Double] = List.tabulate(length + 3)(x => math.sin(x.toDouble/64.0))
    val outputs = inputs.map(x => Constant(x,entity.z.fixed))

    entity.x           --> (inputs.map(Constant(_)))        // Assign the System Input
    entity.z          <-- (outputs,3,length+3,4)           // Check the System Output


     this.createTest

  }


object IirProject extends Project {
  val location:String = TestConstants.locationPrefix + "language\\iir_output"
  implicit val clk         = ClockControl("clk","reset")

  val len = 3

  val num             = array("num",INPUT,S(8,6))(len)
  val den             = array("den",INPUT,S(8,6))(len)
  val x              = signal("signal_in1",INPUT,S(8,6))
  val z              = signal("signal_out",OUTPUT,S(8,6))

  val iW = S(12,8)

  override val root = new IirEntity("iir",num,den,x,z,iW)
  override val tests    = List(Test(new TestCase(root)))
  override val testType = Some(new Isim(this))

}



  def main(args:Array[String]) = {
    IirProject.createProject2
  }
}