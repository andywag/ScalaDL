package com.simplifide.generate.signal

import com.simplifide.generate.parser.model.SignalType


/**
 * Class defining the type of a signal
 */

/** Class which contains the type of appendSignal related to it's operation
*/
class OpType extends SignalType {

  /** Returns a list of appendSignal declarations associated with this type */
  def reverseType:OpType = {
    if (this.isOutput) OpType.Input
    else if (this.isInput) OpType.Output
    else this
  }
  /** Converts the type of the signal to append to a testbench */
  def testType:OpType = {
    if (this.isInput) OpType.Register
    else if (this.isOutput) OpType.Signal
    else this
  }


  def isReg:Boolean    = false
  def isOutput:Boolean = false
  def isInput:Boolean  = false
  def isSignal:Boolean = !isOutput && !isInput

}

object OpType {
   object Input extends OpType{
    override def isInput:Boolean = true;
    override def toString = "Input"

  }
  
   object Output extends OpType{
     override def isOutput:Boolean = true;
     override def toString = "Output"

  }
  
   object Signal extends OpType  {
     override def toString = "Signal"
   }
   object Register extends OpType {
     override def isReg:Boolean    = true
   }


   object SignalAndReg extends OpType

   object Constant extends OpType 
   object Param    extends OpType

   object ModuleInput extends OpType {
      override def isInput:Boolean = true;

   }
   
   object ModuleOutput extends OpType {
     override def isOutput:Boolean = true;

   }

   object ModuleRegOutput extends OpType {
     override def isOutput:Boolean = true
     override def isReg:Boolean = true

   }

  
}
