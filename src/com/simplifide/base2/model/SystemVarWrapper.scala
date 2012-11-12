package com.simplifide.base2.model

import com.simplifide.base.core.`var`.realvars.SystemVar
import scala.collection.JavaConversions._
import com.simplifide.generate.signal.{FixedType, OpType, SignalTrait}
import com.simplifide.base2.generator.{ScalaDeclaration, FunctionCall}

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 3/5/12
 * Time: 9:38 AM
 * To change this template use File | Settings | File Templates.
 */

class SystemVarWrapper(val variable:SystemVar) extends SignalTrait {

  override val name   = variable.getname()
  override val opType = {
    val opType = variable.getOpTypeVar.getVariableType
    opType match {
      case SystemVar.INPUT   => OpType.Input
      case SystemVar.OUTPUT  => OpType.Output
      case SystemVar.REG     => OpType.Register
      case SystemVar.SIGNAL  => OpType.Signal
      case _                 => OpType.Signal
    }
  }
  override val fixed = {
    FixedType.unsigned(variable.getWidth,0)
  }

  def newSignal(name:String = this.name,
                opType:OpType = this.opType,
                fix:FixedType = this.fixed):SignalTrait = this



  def createScalaDeclaration = {
    val functionName =  "\"" + name + "\""
    val ioType = opType match {
      case OpType.Input    => "INPUT"
      case OpType.Output   => "OUTPUT"
      case OpType.Signal   => "SIGNAL"
      case OpType.Register => "REGISTER"
    }
    val fixed = "U(" + variable.getWidth + ",0)"
    ScalaDeclaration(name,FunctionCall("signal",List(functionName,ioType,fixed)))
  }

}