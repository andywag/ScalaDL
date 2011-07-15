package com.simplifide.generate.blocks.basic.condition

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import scala.collection.mutable.ListBuffer
import com.simplifide.generate.util.StringOps
import com.simplifide.generate.signal.SignalTrait
import com.simplifide.generate.generator.{SimpleSegment, CodeWriter, SegmentReturn}


/** Case Statement not including the always/process head */
class CaseStatement2(condition:SimpleSegment) extends SimpleSegment{


  val conditions:ListBuffer[CaseCondition2] = new ListBuffer[CaseCondition2]();

  def addCondition(cond:Option[SimpleSegment],result:SimpleSegment) {
    val condition = new CaseCondition2(cond,result);
    conditions += condition;
  }

  

  def createVerilogCodeInternal(writer:CodeWriter, signal:Option[SignalTrait]):SegmentReturn =  {
    def createVerilogHead(writer:CodeWriter, signal:Option[SignalTrait]):String = {
      val build2:StringBuilder = new StringBuilder();
      build2.append("case(");
      build2.append(writer.createCode(condition).code)
      build2.append(")");
      return build2.toString
    }

    def createVerilogBody(writer:CodeWriter, signal:Option[SignalTrait]):String = {
      val build = new StringBuilder();
      for (condition <- conditions) {
        build.append(writer.createCode(condition).code)
      }
      return StringOps.indentLines(build.toString,1)
     }
      
    val build = new StringBuilder();
    build.append(createVerilogHead(writer,signal));
    build.append('\n')
    build.append(createVerilogBody(writer,signal))
    build.append("endcase\n")
    SegmentReturn.segment(build.toString)
  }
  
 



  def createVhdlCodeInternal(writer:CodeWriter,output:Option[SignalTrait]):SegmentReturn = {

    def createVhdlBody(output:Option[SignalTrait]):String = {
      val build = new StringBuilder();
      for (condition <- conditions) {
        build.append(writer.createCode(condition).code )
      }
      return StringOps.indentLines(build.toString,1)
    }

    val build = new StringBuilder();
      build.append("case ");
      
      build.append(writer.createCode(condition).code)
      build.append(" is\n")
      build.append(createVhdlBody(output))
      build.append("end case;\n")
      return SegmentReturn.segment(build.toString)
  }

 def createCCodeInternal(writer:CodeWriter,output:Option[SignalTrait]):SegmentReturn = {

    def createBody(output:Option[SignalTrait]):String = {
      val build = new StringBuilder();
      for (condition <- conditions) {
        build.append(writer.createCode(condition).code )
      }
      return StringOps.indentLines(build.toString,1)
    }

    val build = new StringBuilder();
      build.append("switch(");
      build.append(writer.createCode(condition).code)
      build.append(") {\n")
      build.append(createBody(output))
      build.append("}\n")
      return SegmentReturn.segment(build.toString)
  }
  
 override def createCode(writer:CodeWriter):SegmentReturn = {
    return new SegmentReturn("",List())
  }
  
  override def createVerilogCode(writer:CodeWriter):SegmentReturn = {
    return createVerilogCodeInternal(writer,None)
  }

  
  override def createVhdlCode(writer:CodeWriter):SegmentReturn = {
    return createVhdlCodeInternal(writer,None)
  }

  
  override def createFixedCode(writer:CodeWriter):SegmentReturn = {
    return createCCodeInternal(writer,None)
  }

  
  override def createFloatCode(writer:CodeWriter):SegmentReturn = {
    return createCCodeInternal(writer,None)
  }

  
  
}

class CaseCondition2(val cond:Option[SimpleSegment],result:SimpleSegment) extends SimpleSegment {


  def createVerilogCodeInternal(writer:CodeWriter,signal:Option[SignalTrait]):SegmentReturn = {
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
      val res2 = StringOps.indentLines(resS, 1)
      build.append('\n')
      build.append(res2)
    }

    return SegmentReturn.segment(build.toString)
  }

 def createVhdlCodeInternal(writer:CodeWriter,signal:Option[SignalTrait]):SegmentReturn = {
    val build = new StringBuilder();
    val condS = cond match {
      case Some(x) => writer.createCode(x).code
      case _ => "others"
    }
    val resS  = writer.createCode(result).code

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
  
  def createCCodeInternal(writer:CodeWriter,signal:Option[SignalTrait]):SegmentReturn = {
    val build = new StringBuilder();
    // Create the Condition String
    val condS = cond match {
      case Some(x) => "case " + writer.createCode(x).code + " : "
      case _ => "default :"
    }
    // Create the result String
    val resS  = writer.createCode(result).code + " break;\n"
    // Append the Condition String
    build.append(condS)
    
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
  
  
  /** Creates the code for this segment 
      TODO Should be error added
   */
   override def createCode(writer:CodeWriter):SegmentReturn = {
      return createVerilogCode(writer)
  }

  
  override def createVerilogCode(writer:CodeWriter):SegmentReturn = {
    return createVerilogCodeInternal(writer,None)
  }

  
  override def createVhdlCode(writer:CodeWriter):SegmentReturn = {
    return createVhdlCodeInternal(writer,None)
  }

  
  override def createFixedCode(writer:CodeWriter):SegmentReturn = {
    return createCCodeInternal(writer,None)
  }

  
  override def createFloatCode(writer:CodeWriter):SegmentReturn = {
    return createCCodeInternal(writer,None)
  }


}
