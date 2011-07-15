package com.simplifide.generate.signal


/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/** Class which contains the type of signal related to it's operation
*/
class OpType {
  /** Returns a list of signal declarations associated with this type */  
  //def getSignalDeclaration(signal:SignalNew):List[SignalDeclarationNew] = List()
  //def getIODeclaration(signal:SignalNew):List[SignalDeclarationNew] = List()
  def isOutput:Boolean = false
  def isInput:Boolean  = false;

}

object OpType {
   object Input extends OpType{
    override def isInput:Boolean = true;

  }
  
   object Output extends OpType{
     override def isOutput:Boolean = true;

  }
  
   object Signal extends OpType
   object Signalr extends OpType


   object SignalAndReg extends OpType

   object Constant extends OpType 
  
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
