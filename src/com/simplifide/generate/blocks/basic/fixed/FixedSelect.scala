package com.simplifide.generate.blocks.basic.fixed

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */


import com.simplifide.generate.blocks.basic.operator.Select
import com.simplifide.generate.signal.{SignalTrait, FixedType}
import com.simplifide.generate.generator.{SimpleSegment, CodeWriter, SegmentReturn}

class FixedSelect(val signal:SignalTrait, override val fixed:FixedType) extends SimpleSegment{


  def getShift:Int     = 0
  
  def createCode(writer:CodeWriter,output:SignalTrait):SegmentReturn = {
    createCode(writer)
  }

  /*
  def createCItem(writer:CodeWriter,funcname:String):SegmentReturn = {
    val bot = signal.fixed.frac - fixed.frac
    val top = bot + this.fixed.width - 1
    
    val sig = writer.createCode(signal)
    if (bot == 0) {
      return sig
    }
    else {
      return SegmentReturn.segment(funcname + "(" + sig.code + "," + signal.fixed.width + ","
                                   + signal.fixed.frac + "," + top + ","+ bot + ")")
    }
  }
  
  override def createFloatCode(writer:CodeWriter):SegmentReturn = 
    return createCItem(writer,"select_float")
  
  override def createFixedCode(writer:CodeWriter):SegmentReturn = 
     return createCItem(writer,"select_fixed")
  
  
  override def createFloatCode(writer:CodeWriter,output:SignalTrait):SegmentReturn =
    return createCItem(writer,"select_float")
  
  
  override def createFixedCode(writer:CodeWriter,output:SignalTrait):SegmentReturn =
     return createCItem(writer,"select_fixed")
  */

  
  override def createCode(writer:CodeWriter):SegmentReturn = {
     val bot = signal.fixed.fraction - fixed.fraction + getShift// Bottom of the Select
     val top = bot + this.fixed.width - 1
     val sel = new Select(signal,Some(top),Some(bot))
     val ret =  writer.createCode(sel)
     if (signal.fixed.signed.isSigned) {
       val builder = new StringBuilder
       //builder.append("$signed(")
       builder.append(ret.code)
       //builder.append(")")
       return SegmentReturn.segment(builder.toString)
     }
     else {
       return ret
     }
     
  }
  
}

object FixedSelect {


  /** Creates a new selection of the signal with the input fixed type fixed */
  def newSelect(signal:SignalTrait,fixed:FixedType) =
    new FixedSelect(signal,fixed)

  /**Creates a new selection of the signal with the input fixed type fixed shift
   * by scale
   */
  def newSelect(signal:SignalTrait,fixed:FixedType,scale:Int) =
    new FixedSelect.Scale(signal,fixed,scale)

  class Scale(override val signal:SignalTrait, override val fixed:FixedType, val scale:Int) extends FixedSelect(signal,fixed) {
    override def getShift:Int     = scale
  }
    

  
}
