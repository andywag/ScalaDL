package com.simplifide.generate.signal

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import com.simplifide.generate.generator.{SimpleSegment, CodeWriter, SegmentReturn}

class Constant( val name:String,
                val vector:VectorType,
                override val fixed:FixedType,
               val value:ConstantValue) extends SignalTrait with SimpleSegment{

  override def newSignal(nam:String,optype:OpType,fix:FixedType):SignalTrait = this

  override val opType = OpType.Constant


  
   private def getInteger:Int = {
     val flo = value.getFloatValue(fixed)
     val res:Double = math.round((flo*math.pow(2.0,fixed.fraction)))
     return res.toInt
   } 
  
  def createCSD:List[Constant.CSD] = Constant.createCSD(getInteger)
  


  def createCItem(writer:CodeWriter):SegmentReturn = {
    val flo = value.getFloatValue(fixed)
    val res:Double = math.round((flo*math.pow(2.0,fixed.fraction)))/math.pow(2.0, fixed.fraction)
    return SegmentReturn.segment(res.toString)
  }
  
  override def createFloatCode(writer:CodeWriter):SegmentReturn       = {
    return createCItem(writer)
  }
  
  override def createFixedCode(writer:CodeWriter):SegmentReturn       = {
     return createCItem(writer)
  }
	

  
  override def createVerilogCode(writer:CodeWriter):SegmentReturn = {
    val flo = value.getFloatValue(fixed)
    val res:Double = math.round((flo*math.pow(2.0,fixed.fraction)))
    val ival = res.toInt
    
    val builder = new StringBuilder
    if (ival < 0) builder.append("-")
    builder.append(fixed.width.toString)
    if (fixed.signed.isSigned) builder.append("'sd") else builder.append("'d")

    builder.append(math.abs(ival).toString)
    return SegmentReturn.segment(builder.toString)
  }

   def debugCSDString():String = {
	   val builder = new StringBuilder()
     builder.append("[")
     builder.append(this.value.getFloatValue(this.fixed))
     builder.append(",")
     builder.append(this.getInteger)
     builder.append("]")
     builder.append("(")
     var first = true
     val csds = this.createCSD
     for (csd <- csds) {
        if (!first) builder.append(",")
        builder.append(csd.debugString)
       first = false
     }
     builder.append(")")
     return builder.toString
  }


}

object Constant {

  implicit def SignalTrait2Fixed(signal:SignalTrait):FixedType = signal.fixed
  // Convenience Methods for Construction
  //def newConstant(value:Int,signal:SignalTrait) = newConstant(value,signal.fixed)
  //
  //def newConstant(value:Float,signal:SignalTrait) = newConstant(value,signal.fixed)


  def newConstant(value:Int,fixed:FixedType) =
    new Constant("",VectorType.NoVector,fixed,new ConstantValue.IntegerValue(value))

  def newConstant(value:Float,fixed:FixedType) =
    new Constant("",VectorType.NoVector,fixed,new ConstantValue.FloatValue(value))

  def newConstant(value:Double,fixed:FixedType) =
    new Constant("",VectorType.NoVector,fixed,new ConstantValue.FloatValue(value.toFloat))

  def newConstant(value:Int,width:Int) = new Constant("",VectorType.NoVector,FixedType.unsigned(width,0),new ConstantValue.IntegerValue(value))

  def newIntConstant(value:Int,width:Int) = new Constant("",VectorType.NoVector,FixedType.unsigned(width,0),new ConstantValue.IntegerValue(value))


  abstract class MaxMin(override val fixed:FixedType) extends Constant("",VectorType.NoVector,fixed,new ConstantValue.FloatValue((0.0).toFloat)) {
     
    val sign:String = ""
    private def getMaxValue:Int = {
      val res:Double = (math.pow(2.0,fixed.width-1)-1)
      return res.toInt
    }
    
    override def createVerilogCode(writer:CodeWriter):SegmentReturn = {
      val ival = getMaxValue
      val builder = new StringBuilder
      builder.append(sign)
      builder.append(fixed.width.toString)
      builder.append("'d")
      builder.append(ival.toString)
      return SegmentReturn.segment(builder.toString)
    }
  }
  
  class Integer(override val name:String,
		     override val vector:VectorType,
		     override val fixed:FixedType,
		     val intValue:Int) extends Constant(name,vector,fixed,new ConstantValue.IntegerValue(intValue)) {
	  override def createCItem(writer:CodeWriter):SegmentReturn = {
	 	  new SegmentReturn(intValue.toString,List())
	  }
	   override def createVerilogCode(writer:CodeWriter):SegmentReturn = {
			  val ival = intValue
			  val builder = new StringBuilder
			  builder.append(fixed.width.toString)
			  builder.append("'d")
			  builder.append(ival.toString)
			  return SegmentReturn.segment(builder.toString)
	  }
  }
  
  class Max(override val fixed:FixedType) extends MaxMin(fixed) {
    
  }
  
  class Min(override val fixed:FixedType) extends MaxMin(fixed) {
    override val sign:String = "-"
  }
  
  def createCSD(value:Int):List[CSD] = {
     if (value == 0) return List()
    
     val avalue:Int = math.abs(value)    // Integer Value of the CSD
     val sign:Int = math.signum(value)   // Sign of the CSD
    
     val l2:Int = math.ceil(math.log(avalue)/math.log(2.0)).toInt
     val pos:Int = -math.pow(2.0,l2).toInt   + avalue        // Finds the positive value
     val neg:Int = -math.pow(2.0,l2-1).toInt + avalue        // Finds the negative value
     
     if (pos == 0) return List(new CSD(sign < 0,math.abs(l2).toInt))
     if (neg == 0) return List(new CSD(sign < 0,math.abs(l2-1).toInt))
     
     val pc:List[CSD] = Constant.createCSD(sign*pos)
     val nc:List[CSD] = Constant.createCSD(sign*neg)
     
     if (pc.size < nc.size) return List(new CSD(sign < 0,math.abs(l2).toInt)) ::: pc
     else return List(new CSD(sign < 0,math.abs(l2-1).toInt)) ::: nc
    
   }
  
  class CSD(val negative:Boolean,val value:Int) {
    def debugString:String = if (negative) "-" + value.toString else value.toString
  }
  

  
}

