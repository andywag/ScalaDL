package com.simplifide.generate.blocks.basic.flop

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import com.simplifide.generate.signal.{SignalTrait, OpType, Signing, FixedType}
import com.simplifide.generate.generator.{SimpleSegment, CodeWriter, SegmentReturn}


class Clocks {

}
/** Class which contains classes and methods for dealing with clocks */
object Clocks {

  /** Top Level Class for a clock signal */
  class ClockSignal(override val name:String) extends SimpleSegment{
    override def createCode(writer:CodeWriter):SegmentReturn = SegmentReturn(name)
  }
  /** Index of Clock. Used for Time Sharing
   *
   * @constructor
   * @parameter name String
   * @parameter index Index
   **/
  class Index(name:String,index:Int) extends ClockSignal(name) {
    def getSignal:SignalTrait =
      SignalTrait(name,OpType.Input,new FixedType.Main(Signing.UnSigned,index,0))
  }

  /**
   * Class defining a clock
   *
   * @constructor
   * @parameter name Name of Clock Signal
   * @parameter posedge True if the signal is valid on the positive edge
   *
   */
  class Clock(name:String, posedge:Boolean) extends ClockSignal(name){
  /** Returns the appendSignal associated with this clock */
    def sensitivityList():List[SimpleSegment] = {
      if (posedge) return List(new ClockEdgeHead(this, "posedge "))
      else         return List(new ClockEdgeHead(this, "negedge "))
    }
  }
   /**
   * Class defining a reset with options whether it is synchronous or not
   *
   * @constructor
   * @parameter name Name of Clock Signal
   * @parameter async True if the signal is an asynchronous signal
   * @parameter posedge True if the signal is valid on the positive edge
   *
   */
  class Reset(name:String,async:Boolean = false,val activeLow:Boolean=false) extends ClockSignal(name) {
    def sensitivityList():List[SimpleSegment] = {
      if (async) {
        if (activeLow) return List(new ClockEdgeHead(this, "negedge "))
        else           return List(new ClockEdgeHead(this, "posedge "))
      }
    return List()
    }
  }

  /** Enable Signal for the Clock */
  class Enable(override val name:String) extends ClockSignal(name)

  /**
   * Class which creates part of the sensitivity list for signal edges
   */

  class ClockEdgeHead(signal:ClockSignal, edge:String) extends SimpleSegment {

    override def createCode(writer:CodeWriter):SegmentReturn =
      return SegmentReturn(edge) + writer.createCode(signal)

  }
  
}