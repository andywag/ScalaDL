package com.simplifide.generate.generator

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

class InterfaceMessageItem {
  
  val isCode:Boolean      = false
  
  val isTopError:Boolean  = false
  val isError:Boolean     = false
  val isWarning:Boolean   = false
  val isInfo:Boolean      = false
  
  def getCode:String      = "" 
  def getLine:Int         = 0
  def getMessage:String   = ""
  
  
  
}

object InterfaceMessageItem {

   class Code(val code:String) extends InterfaceMessageItem{
     override val isCode:Boolean = true;
     override def getCode:String = code
   }

}