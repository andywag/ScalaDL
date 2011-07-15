package com.simplifide.generate.blocks.basic.condition

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import com.simplifide.generate.generator.CodeWriter
import com.simplifide.generate.generator.SegmentReturn
import com.simplifide.generate.generator.SimpleSegment
import com.simplifide.generate.signal.SignalTrait
import com.simplifide.generate.blocks.basic.SimpleStatement


/** Simple Question Statement */
class SimpleMux(val condition:SimpleSegment,val tr:SimpleSegment,val fa:SimpleSegment) extends SimpleSegment{



  override def createCode(writer:CodeWriter):SegmentReturn = {
    val builder = new StringBuilder()
    builder.append("(")
    builder.append(condition.createVerilogCode(writer))
    builder.append(")")
    builder.append(" ? ")
    builder.append(tr.createVerilogCode(writer))
    builder.append(" : ")
    builder.append(fa.createVerilogCode(writer))
    
    return SegmentReturn.segment(builder.toString)
  }
  
}

object SimpleMux {

  def newMux(condition:SimpleSegment,tr:SimpleSegment,fa:SimpleSegment) =new SimpleMux(condition,tr,fa)

  def statement(output:SignalTrait,condition:SimpleSegment,tr:SimpleSegment, fa:SimpleSegment):SimpleStatement = {
    val mux = newMux(condition,tr,fa)
    val stat = new SimpleStatement.Assign(output,mux)
    return stat
  }
}
