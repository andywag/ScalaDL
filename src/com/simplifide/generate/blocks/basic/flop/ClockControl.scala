package com.simplifide.generate.blocks.basic.flop

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */


import scala.collection.mutable.ListBuffer
import com.simplifide.generate.generator.{CodeWriter, SimpleSegment}
import com.simplifide.generate.signal._
import com.simplifide.generate.parser.model.Expression
import com.simplifide.generate.parser.factory.CreationFactory

/**
 * Class which defines how a register will operate.
 *
 * @constructor
 * @parameter name Name of Clock Control
 * @parameter clock Clock
 * @parameter reset Optional Clock Reset
 * @parameter enable Optional Clock Enable
 * @parameter index Optional Index for the Clock - Used for Sharing
 */
class ClockControl(override val name:String,
  val clock:Clocks.Clock,
  val reset:Option[Clocks.Reset]   = None,
  val enable:Option[Clocks.Enable] = None,
  val index:Option[Clocks.Index]   = None,
  val period:Int                   = 10) extends SimpleSegment with com.simplifide.generate.parser.model.Clock{

  override val delay = 0
  override def createCode(implicit writer:CodeWriter) = null

  def withOutReset = new ClockControl("",this.clock)
  /** Create a new version of the clock control with an enable */
  def createEnable(enable:SignalTrait) = new ClockControl("",this.clock,this.reset,Some(new Clocks.Enable(enable.name)))
  /** */
  /*def createEnable(enable:Expression)(implicit writer:CodeWriter,creator:CreationFactory) =
    new ClockControl("",this.clock,this.reset,Some(new Clocks.Enable(enable.create.createCode.code)))
  */

  /** Returns the appendSignal associated with the clock */
  def clockSignal(optype:OpType = OpType.Input):SignalTrait =
    SignalTrait(clock.name,optype,FixedType.Simple)

  /** Returns the appendSignal associated with the reset */
  def resetSignal(optype:OpType = OpType.Input):Option[SignalTrait] =
    reset.map(x => SignalTrait(x.name,optype,FixedType.Simple))

  /** Returns the appendSignal associated with the reset */
  def enableSignal(optype:OpType = OpType.Input):Option[SignalTrait] =
    enable.map(x => SignalTrait(x.name,optype,FixedType.Simple))

  /** All signals included with this class */
  def allSignals(optype:OpType):List[SignalTrait] = {
    val buffer = new ListBuffer[SignalTrait]()
    buffer.append(clockSignal(optype))
    resetSignal(optype).map(x => buffer.append(x))
    enableSignal(optype).map(x => buffer.append(x))
    return buffer.toList
  }

  def input = allSignals(OpType.Input)

  /** Creates a bus based on the signals defined in this clock */
  def getBus(opType:OpType = OpType.Input):Bus[BusType] = new Bus[BusType]("",BusType(allSignals(opType)))


  /** Returns the sensitivity list for this */
  def createSensitivityList():List[SimpleSegment] =
    clock.sensitivityList() ::: (if (reset.isDefined) reset.get.sensitivityList else List())

}

object ClockControl {

  def apply(clock:String = "clk", reset:String = "reset", enable:String = "",posedge:Boolean = true, reset_sync:Boolean = false, period:Int = 10):ClockControl = {
    new ClockControl("",
                     new Clocks.Clock(clock,posedge),
                     if (reset.equalsIgnoreCase(""))  None else Some(new Clocks.Reset(reset,reset_sync, reset_sync)),
                     if (enable.equalsIgnoreCase("")) None else Some(new Clocks.Enable(enable)),
                     None,
      period
                    )
  }



}
