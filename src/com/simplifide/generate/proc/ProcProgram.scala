package com.simplifide.generate.proc

import collection.mutable.ListBuffer
import com.simplifide.generate.generator.SimpleSegment
import com.simplifide.generate.blocks.basic.SimpleStatement
import com.simplifide.generate.signal.SignalTrait
import com.simplifide.generate.proc.ProcProgram.SignalAssign
import com.simplifide.generate.parser.condition.Case
import com.simplifide.generate.parser.model.{Expression, Clock}
import com.simplifide.generate.parser.{BaseParser, ObjectFactory, ModuleParser}
import com.simplifide.generate.project2.Module

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 9/19/11
 * Time: 1:42 PM
 * To change this template use File | Settings | File Templates.
 */

class ProcProgram(val module:Module) extends ModuleParser {

  implicit val base = this

  val signalAssigns = new ListBuffer[SignalAssign]()


  /** Find a list of controls for the processor */
  def controls:List[Controls] = {
    val controls = module.statements.map(x => x.asInstanceOf[SimpleSegment]).flatMap(_.controls).toList
    System.out.println("Controls" + controls)
    controls
  }

  /** Create a complete map of the control signals */
  def controlMap:List[Controls] = {
     null
  }

  /** Parse the Source File and find a list of Controls */
  def parse:List[List[Controls]] = {
    def handleStatement(signalAssign:SignalAssign):List[Controls] = {
      if (signalAssign.state == None) return List() // Filter out when there isn't a statement (should be error)
      val statement = signalAssign.state.get

      val state = module.getStatement(statement.output.asInstanceOf[SignalTrait])
      state match {
        case Some(x) => x.createControl(statement,module,signalAssign.index)
        case None => List()
      }
    }
    val controls = this.signalAssigns.toList.map(x => handleStatement(x))

    System.out.println("Controls" + controls)
    controls
  }

  /** Convenience Conversion to and From Signal */
  implicit def Signal2SignalAssign(signal:SignalTrait):SignalAssign = new ProcProgram.SignalAssign(signal,-1,None)
  implicit def SignalAssign2Signal(signal:ProcProgram.SignalAssign):SignalTrait = signal.signal


}

object ProcProgram {

  /** Class which supports holding the index of the program instruction */
  class SignalAssign(val signal:SignalTrait, val index:Int, val state:Option[SimpleStatement]) {
    def ~>(ind:Int) = new SignalAssign(signal,ind,None)

    def <:= (rhs:Expression)(implicit base:ProcProgram):Expression = {
      val state = signal.createStatement(rhs)
      base.signalAssigns.append(new SignalAssign(this.signal,index,state))
      null
    }

  }


}