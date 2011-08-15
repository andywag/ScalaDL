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
import com.simplifide.generate.parser.model.Expression
import com.simplifide.generate.parser.math.Adder._
import com.simplifide.generate.parser.{ObjectFactory, ExpressionReturn}
import com.simplifide.generate.language.Conversions._


/** Simple Question Statement */
class SimpleMux(val condition:SimpleSegment,val tr:SimpleSegment,val fa:SimpleSegment) extends SimpleSegment{

  override def numberOfChildren = SimpleSegment.maxChildren(List(condition,tr,fa))
  override def child(i:Int) = new SimpleMux(condition.child(i),tr.child(i),fa.child(i))

    override def split(output:Expression,index:Int):ExpressionReturn = {

    val out   = (if (index == -1) output else output.copy(index)).asInstanceOf[SimpleSegment]
    val lp    = tr.split(out,0)
    val rp    = fa.split(out,1)
    val mux = new SimpleStatement.Assign(out,new SimpleMux(condition,lp.output.asInstanceOf[SimpleSegment],rp.output.asInstanceOf[SimpleSegment]))

    new ExpressionReturn(out,lp.states ::: rp.states ::: List(mux)  )
  }

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

  def apply(condition:SimpleSegment,tr:SimpleSegment,fa:SimpleSegment) =new SimpleMux(condition,tr,fa)

  def statement(output:SignalTrait,condition:SimpleSegment,tr:SimpleSegment, fa:SimpleSegment):SimpleStatement = {
    val mux = SimpleMux(condition,tr,fa)
    val stat = new SimpleStatement.Assign(output,mux)
    return stat
  }
}
