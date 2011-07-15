package com.simplifide.generate.blocks.basic.condition

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import scala.collection.mutable.ListBuffer
import com.simplifide.generate.blocks.basic.state.AlwaysProcess
import com.simplifide.generate.blocks.basic.SimpleStatement
import com.simplifide.generate.signal.{Constant, VectorType, SignalTrait}
import com.simplifide.generate.generator._

class StatementMux(val condition:SimpleSegment,
                   val output:SignalTrait,
                   val inputs:List[SimpleSegment]) extends SimpleSegment{


  override def createCode(writer:CodeWriter):SegmentReturn = {
    /*
    val cas = new CaseStatement2(condition)
    var index = 0;
     
    val errors = new ListBuffer[InterfaceError]()
    val extra  = new ListBuffer[SimpleStatement]()
    for (input <- inputs) {
    	val cond = new Constant.Integer("",VectorType.NoVector,condition.fixed,index)
    	val sret:SegmentReturn = writer.createCode(input)
    	 
    	val uin = BasicSegments.Ident(sret.code)
    	errors.appendAll(sret.errors)
    	//extra.appendAll(sret.extraStatements)
    	val ass  = new SimpleSegment.Reg(output,uin)
    	cas.addCondition(Some(cond), ass)
    	index = index + 1;
    }
    val alw = AlwaysProcess.Star(None,cas,List())
    
    var ret = new SegmentReturn("",List[InterfaceError]())
    for (state <- extra) ret = ret.combine(writer.createCode(state))
    
    ret = ret.combine(writer.createCode(alw))
    return ret
    */
    return null
  }
  

  
}
