package com.simplifide.generate.blocks.basic.condition

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import com.simplifide.generate.util.StringOps
import scala.collection.mutable.ListBuffer
import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, BaseCodeSegment, SimpleSegment}

/** @deprecated : Use ConditionStatement 2 instead. This is only used for existing flops */
class ConditionStatement extends BaseCodeSegment{
  val statements = new ListBuffer[ConditionStatement.IfElseClause]();

  def addClause(condition:Option[SimpleSegment],body:SimpleSegment) {
    var clause:ConditionStatement.IfElseClause = null
    if (statements.length == 0) clause = new ConditionStatement.IfElseFirst(condition,body)
    else if (condition == None) clause = new ConditionStatement.IfElseLast(body)
    else                        clause = new ConditionStatement.IfElseClause(condition,body)
    statements += clause;
  }

  override def createVerilogCode(writer:CodeWriter):SegmentReturn =  {
    val build = new StringBuilder();
    for (statement <- statements) {
      build.append(statement.createVerilogCode(writer));
    }
    return SegmentReturn.segment(build.toString)
  }

   override def createVhdlCode(writer:CodeWriter):SegmentReturn =  {
    val build = new StringBuilder();
    for (statement <- statements) {
      build.append(statement.createVhdlCode(writer));
    }
    build.append("end if;\n");
    return SegmentReturn.segment(build.toString)
  }

}

object ConditionStatement {

  def IfElseClause(condition:Option[SimpleSegment],body:SimpleSegment):IfElseClause = new IfElseClause(condition,body)
  def IfElseFirst(condition:Option[SimpleSegment],body:SimpleSegment):IfElseClause =new IfElseFirst(condition,body)
  def IfElseLast(body:SimpleSegment):IfElseClause = new IfElseLast(body)

  class IfElseClause(condition:Option[SimpleSegment],body:SimpleSegment) extends BaseCodeSegment {
   override def createVerilogCode(writer:CodeWriter):SegmentReturn =  {
      val build = new StringBuilder();
      build.append("else if (");
      build.append(condition.get.createVerilogCode(writer).code);
      build.append(") begin\n")
      build.append(StringOps.indentLines(body.createVerilogCode(writer).code, 1))
      build.append("end\n");
      return SegmentReturn.segment(build.toString)
    }
     override def createVhdlCode(writer:CodeWriter):SegmentReturn ={
      val build = new StringBuilder();
      build.append("elsif (");
      build.append(condition.get.createVhdlCode(writer).code);
      build.append(") then\n")
      build.append(StringOps.indentLines(body.createVhdlCode(writer).code, 1))
      //build.append("end if;");
      return SegmentReturn.segment(build.toString)
    }
  }

  class IfElseFirst(condition:Option[SimpleSegment],body:SimpleSegment) extends IfElseClause(condition,body) {

    override def createVerilogCode(writer:CodeWriter):SegmentReturn =  {
      val build = new StringBuilder();
      build.append("if (");
      build.append(condition.get.createVerilogCode(writer).code);
      build.append(") begin\n")
      build.append(StringOps.indentLines(body.createVerilogCode(writer).code, 1))
      build.append("end\n");
      return SegmentReturn.segment(build.toString)
    }

    override def createVhdlCode(writer:CodeWriter):SegmentReturn =  {
      val build = new StringBuilder();
      build.append("if (");
      build.append(condition.get.createVhdlCode(writer).code);
      build.append(") then\n")
      build.append(StringOps.indentLines(body.createVhdlCode(writer).code, 1))
      return SegmentReturn.segment(build.toString)
    }
  }

  class IfElseLast(body:SimpleSegment) extends IfElseClause(None,body) {
     override def createVerilogCode(writer:CodeWriter):SegmentReturn =  {
      val build = new StringBuilder();
      build.append("else begin\n")
      build.append(StringOps.indentLines(body.createVerilogCode(writer).code, 1))
      build.append("end\n");
      return SegmentReturn.segment(build.toString)
    }
     override def createVhdlCode(writer:CodeWriter):SegmentReturn =  {
      val build = new StringBuilder();
      build.append("else\n")
      build.append(StringOps.indentLines(body.createVhdlCode(writer).code, 1))
      return SegmentReturn.segment(build.toString)
    }
  }

}


