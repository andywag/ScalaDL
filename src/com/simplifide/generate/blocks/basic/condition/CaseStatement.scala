package com.simplifide.generate.blocks.basic.condition

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import com.simplifide.generate.generator.BaseCodeSegment
import scala.collection.mutable.ListBuffer
import com.simplifide.generate.generator.CodeWriter
import com.simplifide.generate.generator.SegmentReturn
import com.simplifide.generate.util.StringOps
import com.simplifide.generate.generator.SimpleSegment


/** Case Statement not including the always/process head 
    @deprecated use the statement version instead
 */
class CaseStatement(condition:SimpleSegment) extends BaseCodeSegment{

  val conditions:ListBuffer[CaseStatement.CaseCondition] = new ListBuffer[CaseStatement.CaseCondition]();

  def addCondition(cond:Option[SimpleSegment],result:SimpleSegment) {
    val condition = new CaseStatement.CaseCondition(cond,result);
    conditions += condition;
  }

  private def createVerilogHead(writer:CodeWriter):String = {
      val build2:StringBuilder = new StringBuilder();
      build2.append("case(");
      build2.append(this.condition.createVerilogCode(writer))
      build2.append(")");
      return build2.toString
  }

  private def createVerilogBody(writer:CodeWriter):String = {
    val build = new StringBuilder();
     for (condition <- conditions) {
        build.append(condition.createVerilogCode(writer).code)
      }
      return StringOps.indentLines(build.toString,1)
  }

  override def createVerilogCode(writer:CodeWriter):SegmentReturn =  {
      val build = new StringBuilder();
      build.append(createVerilogHead(writer));
      build.append('\n')
      build.append(this.createVerilogBody(writer))
      build.append("endcase\n")
      SegmentReturn.segment(build.toString)
  }



  override def createVhdlCode(writer:CodeWriter):SegmentReturn = {

    def createVhdlBody():String = {
    val build = new StringBuilder();
     for (condition <- conditions) {
        build.append(condition.createVhdlCode(writer).code)
      }
      return StringOps.indentLines(build.toString,1)
    }

    val build = new StringBuilder();
      build.append("case ");
      build.append(this.condition.createVhdlCode(writer).code)
      build.append(" is\n")
      build.append(createVhdlBody)
      build.append("end case;\n")
      return SegmentReturn.segment(build.toString)
  }


}

object CaseStatement {



  class CaseCondition(val cond:Option[SimpleSegment],result:SimpleSegment) extends BaseCodeSegment {


    override def createVerilogCode(writer:CodeWriter):SegmentReturn = {
      val build = new StringBuilder(16);
      val condS = cond match {
        case Some(x) => x.createVerilogCode(writer).code;
        case _ => "default"
      }
      val resS  = result.createVerilogCode(writer).code

      build.append(condS)
      build.append(" : ")

      val sp = resS.split("\n")
      if (sp.length == 1) {
        build.append(resS)
      }
      else {
        val res2 = StringOps.indentLines(resS, 1)
        build.append('\n')
        build.append(res2)
      }

      return SegmentReturn.segment(build.toString)
    }

    override def createVhdlCode(writer:CodeWriter):SegmentReturn = {
      val build = new StringBuilder();
      val condS = cond match {
        case Some(x) => x.createVhdlCode(writer).code;
        case _ => "others"
      }
      val resS  = result.createVhdlCode(writer).code;

      build.append("when ")
      build.append(condS)
      build.append(" => ")

      val sp = resS.split("\n")
      if (sp.length == 1) {
        build.append(resS)
      }
      else {
        val res2 = StringOps.indentLines(resS, 1)
        build.append('\n')
        build.append(res2)
      }

      return SegmentReturn.segment(build.toString)
    }

  }

}

