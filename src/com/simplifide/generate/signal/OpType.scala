package com.simplifide.generate.signal

import com.simplifide.generate.parser.model.SignalType


/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/** Class which contains the type of signal related to it's operation
*/
class OpType extends SignalType {
  /** Returns a list of signal declarations associated with this type */  
  //def getSignalDeclaration(signal:SignalNew):List[SignalDeclarationNew] = List()
  //def getIODeclaration(signal:SignalNew):List[SignalDeclarationNew] = List()
  def isReg:Boolean    = false
  def isOutput:Boolean = false
  def isInput:Boolean  = false
  def isSignal:Boolean = !isOutput && !isInput

}

object OpType {
   object Input extends OpType{
    override def isInput:Boolean = true;

  }
  
   object Output extends OpType{
     override def isOutput:Boolean = true;

  }
  
   object Signal extends OpType
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
     override def isOutput:Boolean = true;

   }

  
}
