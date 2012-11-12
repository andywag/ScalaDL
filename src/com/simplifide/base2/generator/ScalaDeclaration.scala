package com.simplifide.base2.generator

import com.simplifide.base2.generator.ScalaDeclaration.Impl

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 3/5/12
 * Time: 9:33 AM
 * To change this template use File | Settings | File Templates.
 */

trait ScalaDeclaration extends ScalaObject {
  val name:String
  val value:ScalaObject

  def generate = {
    val header = this match {
      case x:ScalaDeclaration.Override => "  override "
      case _                           => "  "
    }
    header + "val " + name + " = " + value.generate + "\n"
  }

}

object ScalaDeclaration {
  def apply(name:String,  value:ScalaObject) = new Impl(name,value)
  def Override(name:String,  value:ScalaObject) = new Override(name,value)
  
  class Impl(val name:String,val value:ScalaObject) extends ScalaDeclaration
  class Override(val name:String,val value:ScalaObject) extends ScalaDeclaration

}