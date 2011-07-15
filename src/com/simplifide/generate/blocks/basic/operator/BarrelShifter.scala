package com.simplifide.generate.blocks.basic.operator

import com.simplifide.generate.blocks.basic.condition.CaseStatement2
import collection.mutable.ListBuffer
import com.simplifide.generate.blocks.basic.flop.FlopControl
import com.simplifide.generate.blocks.basic.state.AlwaysProcess
import com.simplifide.generate.generator.{SimpleSegment, CodeWriter, SegmentReturn}
import com.simplifide.generate.signal.SignalTrait
import com.simplifide.generate.blocks.basic.SimpleStatement
import com.simplifide.generate.blocks.basic.fixed.FixedSelect

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 3/20/11
 * Time: 10:30 AM
 * To change this template use File | Settings | File Templates. 
 */

class BarrelShifter( val name:String,
                     val clk:FlopControl,
                    val signalIn:SignalTrait,
                    val signalOut:SignalTrait,
                    val condition:SignalTrait,
                    val range:Int) extends SimpleSegment {

  val inputs:List[SignalTrait] = List(signalIn,condition)
  /** Returns a list of outputs for this module */
  val outputs:List[SignalTrait] = List(signalOut)
  /** Returns a list of internal signals for this module */
  val signals:List[SignalTrait] = List()


  override def createCode(writer:CodeWriter):SegmentReturn = {

      /*
      val segments = new ListBuffer[SimpleSegment]()
      if (range == 0) {
        segments.append(new SimpleStatement.Assign(signalOut,FixedSelect.newSelect(signalIn,signalOut.getRealFixedType)))
      }
      else {
        val rcas = new CaseStatement2(condition)
        for (j <- 0 until range) {
          val rs = new SimpleStatement.Assign(signalOut,FixedSelect.newSelect(signalIn,signalOut.getRealFixedType,j))
          rcas.addCondition(Some(condition.createConstant(j)),rs)
        }
        rcas.addCondition(None,new SimpleStatement.Assign(signalOut,signalOut.createConstant(0)))
        segments.append(AlwaysProcess.Star(rcas))
      }

      return SegmentReturn.combineBufferReturns(writer,segments)
      */
      null


  }

}