package com.simplifide.generate.generator

/**
 * Error, Warning or Messages which are used for SegmentReturn
 */

abstract class InterfaceError(override val line:Int,
                              override  val message:String) extends InterfaceMessageItem{


}

object InterfaceError {
  
   class Error(line:Int,message:String) extends InterfaceError(line,message) {
     override val isError:Boolean    = true
   }
   class Warning(line:Int, message:String) extends InterfaceError(line,message) {
     override val isWarning:Boolean   = true
   }
   class Info(line:Int, message:String) extends InterfaceError(line,message) {
     override val isInfo:Boolean   = true
   }
   /** Real Location for the Error */
   def realOffset(message:String,offset:Int):Int = 0
  
}
