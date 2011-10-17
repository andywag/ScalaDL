package com.simplifide.generate.blocks.basic.flop

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */


import scala.collection.mutable.ListBuffer
import com.simplifide.generate.generator.{CodeWriter, SimpleSegment, BaseCodeSegment}
import com.simplifide.generate.signal._

class ClockControl(override val name:String,
                  val clock:Clocks.Clock,
                  val reset:Option[Clocks.Reset] = None,
                  val enable:Option[Clocks.Enable] = None,
                  val index:Option[Clocks.Index] = None) extends SimpleSegment with com.simplifide.generate.parser.model.Clock{

  override val delay = 0
  override def createCode(writer:CodeWriter) = null

  def createEnable(enable:SignalTrait) = new ClockControl("",this.clock,this.reset,Some(new Clocks.Enable(enable.name)))

  /** Returns the appendSignal associated with the clock */
  def clockSignal(optype:OpType = OpType.Input):SignalTrait =
    SignalTrait(clock.name,optype,FixedType.Simple)

  /** Returns the appendSignal associated with the reset */
  def resetSignal(optype:OpType = OpType.Input):Option[SignalTrait] =
    reset.map(x => SignalTrait(x.name,optype,FixedType.Simple))

  /** Returns the appendSignal associated with the reset */
  def enableSignal(optype:OpType = OpType.Input):Option[SignalTrait] =
    enable.map(x => SignalTrait(x.name,optype,FixedType.Simple))

  def allSignals(optype:OpType):List[SignalTrait] = {
    val buffer = new ListBuffer[SignalTrait]()
    buffer.append(clockSignal(optype))
    resetSignal(optype).map(x => buffer.append(x))
    enableSignal(optype).map(x => buffer.append(x))
    return buffer.toList

  }

  def getBus(opType:OpType = OpType.Input):Bus = new Bus("",BusType(allSignals(opType)))



  def createSensitivityList():ListBuffer[BaseCodeSegment] = {

    val lis = new ListBuffer[BaseCodeSegment]()
    lis.append(clock.getSensitivityListItem())
    this.reset match {
      case Some(s) => s.getSensitivityListItem match {case Some(x) =>  lis.append(x); case _ =>}
      case _ =>
    }
    return lis
  }
}

object ClockControl {

  def apply(clock:String, reset:String = "", enable:String = "",posedge:Boolean = true, reset_sync:Boolean = false):ClockControl = {
    new ClockControl("",
                     new Clocks.Clock(clock,posedge),
                     if (reset.equalsIgnoreCase(""))  None else Some(new Clocks.Reset(reset,reset_sync, reset_sync)),
                     if (enable.equalsIgnoreCase("")) None else Some(new Clocks.Enable(enable)),
                     None
                    )
  }

  def default:ClockControl = {
    val clock1 = new Clocks.Clock("clk", true);
    val reset1 = Some(new Clocks.Reset("rst", false, false));
    val enable1 = Some(new Clocks.Enable("enable"));

    return new ClockControl("flop",clock1,reset1,enable1,None)
  }



}
