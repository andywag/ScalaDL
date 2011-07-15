package com.simplifide.generate.blocks.basic.flop

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */


import scala.collection.mutable.ListBuffer
import com.simplifide.generate.signal.{SignalTrait, BusTrait, OpType, FixedType}
import com.simplifide.generate.generator.{CodeWriter, SimpleSegment, BaseCodeSegment}

class FlopControl(val name:String,
                  val clock:Clocks.Clock,
                  val reset:Option[Clocks.Reset],
                  val enable:Option[Clocks.Enable],
                  val index:Option[Clocks.Index]) extends SimpleSegment{

  override def createCode(writer:CodeWriter) = null

  def getClock():Clocks.Clock  = {return clock}
  def getReset():Option[Clocks.Reset]  = {return reset}
  def getEnable():Option[Clocks.Enable] = {return enable}
  def getIndex:Option[Clocks.Index] = index


  /** Returns the signal associated with the clock */
  def getClockSignal(optype:OpType):SignalTrait =
    SignalTrait(clock.name,optype,FixedType.Simple)

  /** Returns the signal associated with the reset */
  def getResetSignal(optype:OpType):Option[SignalTrait] =
    reset.map(x => SignalTrait(x.name,optype,FixedType.Simple))

  /** Returns the signal associated with the reset */
  def getEnableSignal(optype:OpType):Option[SignalTrait] =
    enable.map(x => SignalTrait(x.name,optype,FixedType.Simple))

  def getAllSignals(optype:OpType):List[SignalTrait] = {
    val buffer = new ListBuffer[SignalTrait]()
    buffer.append(getClockSignal(optype))
    getResetSignal(optype).map(x => buffer.append(x))
    getEnableSignal(optype).map(x => buffer.append(x))
    return buffer.toList
  }

  def getBus(opType:OpType = OpType.ModuleInput):BusTrait = BusTrait.newBus("clk",getAllSignals(opType))



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

object FlopControl {
  def default:FlopControl = {
    val clock1 = new Clocks.Clock("clk", true);
    val reset1 = Some(new Clocks.Reset("rst", true, true));
    val enable1 = Some(new Clocks.Enable("enable"));

    return new FlopControl("flop",clock1,reset1,enable1,None)
  }
}
