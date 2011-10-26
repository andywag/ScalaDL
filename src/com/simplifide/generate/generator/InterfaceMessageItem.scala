package com.simplifide.generate.generator


// TODO Remove this class and move it to InterfaceError
/**
 * Class which defines an interface message which is used for error messages
 */

class InterfaceMessageItem {

  val isError:Boolean     = false
  val isWarning:Boolean   = false
  val isInfo:Boolean      = false

  /** Line number where error occured */
  def line:Int         = 0
  /** Error Message */
  def message:String   = ""
  
  
  
}

object InterfaceMessageItem {



}