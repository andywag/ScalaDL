package com.simplifide.generate.blocks.basic.condition

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import scala.collection.mutable.ListBuffer
import com.simplifide.generate.util.StringOps
import com.simplifide.generate.signal.SignalTrait
import com.simplifide.generate.generator.{SimpleSegment, CodeWriter, SegmentReturn}
import com.simplifide.generate.parser.model.Expression
import com.simplifide.generate.parser.condition.Case


/** Case Statement not including the always/process head */
class NewCaseStatement(val condition:SimpleSegment, val statements:List[SimpleSegment]) extends SimpleSegment{



  

  def createCode(writer:CodeWriter):SegmentReturn =  {
    def createVerilogHead(writer:CodeWriter):String = {
      val build2:StringBuilder = new StringBuilder();
      build2.append("case(");
      build2.append(writer.createCode(condition).code)
      build2.append(")");
      return build2.toString
    }

    def createVerilogBody(writer:CodeWriter):String = {
      val build = new StringBuilder();
      for (statement <- statements) {
        build.append(writer.createCode(statement).code)
      }
      return StringOps.indentLines(build.toString,1)
     }
      
    val build = new StringBuilder();
    build.append(createVerilogHead(writer));
    build.append('\n')
    build.append(createVerilogBody(writer))
    build.append("endcase\n")
    SegmentReturn.segment(build.toString)
  }

}

object NewCaseStatement {

  /** Special case for construction from parser */
  def apply(condition:SimpleSegment, statements:List[SimpleSegment]) = {
    new NewCaseStatement(condition,statements)
  }
    /** Special case for construction from parser */
  def newCase(condition:SimpleSegment, statements:List[Expression]) = {
    val caseItems = statements.filter(x => x.isInstanceOf[Case.State]).map(x => x.asInstanceOf[Case.State])
    val items = caseItems.map(NewCaseStatement.Item(_))
    new NewCaseStatement(condition,items)
  }

  object Item {
    def apply(result:SimpleSegment) = new Item(None,result)
    def apply(condition:SimpleSegment, result:SimpleSegment) = new Item(Some(condition), result)
    def apply(cas:Case.State) = new Item(Some(cas.condition.asInstanceOf[SimpleSegment]), cas.result.asInstanceOf[SimpleSegment])
  }

  class Item(val cond:Option[SimpleSegment],result:SimpleSegment) extends SimpleSegment {


    def createCode(writer:CodeWriter):SegmentReturn = {
      val build = new StringBuilder(16);
      val condS = cond match {
        case Some(x) => writer.createCode(x).code
        case _ => "default"
      }
      val resS  = writer.createCode(result).code

      build.append(condS)
      build.append(" : ")

      val sp = resS.split("\n")
      if (sp.length == 1) {
        build.append(resS)
      }
      else {
        //val res2 = StringOps.indentLines(resS, 1)
        //build.append('\n')
        build.append(resS)
      }

      return SegmentReturn.segment(build.toString)
    }



  }

}

