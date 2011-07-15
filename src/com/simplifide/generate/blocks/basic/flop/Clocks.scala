package com.simplifide.generate.blocks.basic.flop

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import com.simplifide.generate.generator.{BaseCodeSegment, CodeWriter, SegmentReturn}
import com.simplifide.generate.signal.{SignalTrait, OpType, Signing, FixedType}

class Clocks {

}

object Clocks {

  class EnableSegment(val segment:BaseCodeSegment) extends Enable("") {
      override def createCode(writer:CodeWriter):SegmentReturn = writer.createCode(segment)
  }

  def defaultFlop(clock:String,reset:String,enable:String):FlopControl = {
    val clock1 = new Clock(clock, true);
    val reset1 = Some(new Reset(reset, true, true));
    val enable1 = Some(new Enable(enable));

    return new FlopControl("flop",clock1,reset1,enable1,None)

  }
  
  class Index(name:String,index:Int) {
    
    def getSignal:SignalTrait = {
      
      SignalTrait(name,OpType.Input,new FixedType.Main(Signing.UnSigned,index,0))
    }
      
    
  }
  class ClockSignal(name:String) extends BaseCodeSegment{

  override def createCode(writer:CodeWriter):SegmentReturn = SegmentReturn.segment(name)

}

class Clock(val name:String, posedge:Boolean) extends ClockSignal(name){
  /** Returns the signal associated with this clock */

  def getSensitivityListItem():BaseCodeSegment = {
    if (posedge) return new ClockEdgeHead(this, "posedge ");
    else         return new ClockEdgeHead(this, "negedge ");
  }

}

class Reset(val name:String,async:Boolean,val activeLow:Boolean) extends ClockSignal(name) {

  def getSensitivityListItem():Option[BaseCodeSegment] = {
    if (async) {
       if (activeLow) return Some(new ClockEdgeHead(this, "negedge "));
       else           return Some(new ClockEdgeHead(this, "posedge "));
    }
    return None;
  }
}

class Enable(val name:String) extends ClockSignal(name) {

}

class ClockEdgeHead(signal:ClockSignal, edge:String) extends BaseCodeSegment{
  override def createVerilogCode(writer:CodeWriter):SegmentReturn = {
    if (edge != null) return SegmentReturn.segment(edge + signal.createVerilogCode(writer))
    return signal.createVerilogCode(writer);
  }

  override def createVhdlCode(writer:CodeWriter):SegmentReturn = {
    return signal.createVhdlCode(writer);
  }
}
  
}