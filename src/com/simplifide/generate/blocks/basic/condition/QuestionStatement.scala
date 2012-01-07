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
import com.simplifide.generate.parser.{ObjectFactory, ExpressionReturn}
import com.simplifide.generate.language.Conversions._


/** Simple Mux which translates to a question mark statement
 *
 *  @constructor Simple Mux Creation
 *  @parameter condition Condition for the statement
 *  @parameter tr True result for the statement
 *  @prameter fa False results for the statement
 *
 * */
class QuestionStatement(val condition:SimpleSegment,val tr:SimpleSegment,val fa:SimpleSegment) extends SimpleSegment{

  override def numberOfChildren = SimpleSegment.maxChildren(List(condition,tr,fa))
  override def child(i:Int) = new QuestionStatement(condition.child(i),tr.child(i),fa.child(i))

  override def split(output:Expression,index:Int):ExpressionReturn = {

    val out   = (if (index == -1) output else output.copy(index)).asInstanceOf[SimpleSegment]
    val cond  = condition.split(out,0)
    val lp    = tr.split(out,1)
    val rp    = fa.split(out,2)
    val mux = new SimpleStatement.Assign(out,new QuestionStatement(cond.output.asInstanceOf[SimpleSegment],
      lp.output.asInstanceOf[SimpleSegment],
      rp.output.asInstanceOf[SimpleSegment]))

    new ExpressionReturn(out,cond.states ::: lp.states ::: rp.states ::: List(mux)  )
  }

  override def createCode(implicit writer:CodeWriter):SegmentReturn = {
    val builder = new StringBuilder()
    builder.append("(")
    builder.append(condition.createVerilogCode(writer))
    builder.append(")")
    builder.append(" ? ")
    builder.append(tr.createVerilogCode(writer))
    builder.append(" : ")
    builder.append(fa.createVerilogCode(writer))
    
    return SegmentReturn(builder.toString)
  }
  
}

/** Factory Methods for Creating of a Simple Mux */
object QuestionStatement {

  def apply(condition:SimpleSegment,tr:SimpleSegment,fa:SimpleSegment) =new QuestionStatement(condition,tr,fa)


}
