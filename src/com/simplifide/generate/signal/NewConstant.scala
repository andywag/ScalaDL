package com.simplifide.generate.signal

import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, SimpleSegment}
import com.simplifide.generate.parser.factory.CreationFactory
import com.simplifide.generate.signal.NewConstant.Long

/**
 * Constant Definition
 */

trait NewConstant extends SimpleSegment {

  val outputType:NewConstant.OutputType = NewConstant.DECIMAL

  
  /** Does a bit slice operation by creating */
  override def apply(index:(Int,Int)) =
    new NewConstant.Long(selectLongValue(index),FixedType.unsigned(index._1-index._2+1,0),outputType)

  
  def selectLongValue(index:(Int, Int)):scala.Long = {
    val long = longValue
    val scaleValue = math.pow(2.0,index._1-index._2+1).toLong-1 << index._2
    val delta      = (longValue & scaleValue) >> index._2
    delta.toLong
  }
  
  
  

  val hex = outputType match {
    case NewConstant.HEX => true
    case _               => false
  }


  def isInteger(double:scala.Double) = math.floor(double) == math.ceil(double)

  def binaryFactor:Option[Int] = {
    this match {
      case x:NewConstant.Long => {
        val number = math.log(x.value)/math.log(2.0)
        if (isInteger(number)) Some(number.toInt) else None
      }
      case x:NewConstant.Double => {
        val number = math.log(x.value)/math.log(2.0)
        if (isInteger(number))
          Some(number.toInt)
        else
          None
      }
    }
  }
  
  def newConstant(fixed:FixedType = this.fixed):SimpleSegment = {
    this match {
      case x:NewConstant.Long   => new NewConstant.Long(x.value,fixed)
      case x:NewConstant.Double => new NewConstant.Double(x.value,fixed)
      case _                    => null
    }
  }

  override def createOutput(output:SimpleSegment)(implicit creator:CreationFactory) = newConstant(output.fixed)

  def longValue:scala.Long = {
    this match {
      case x:NewConstant.Long   => x.value
      case x:NewConstant.Double => (math.pow(2.0,x.fixed.fraction)*x.value).toLong
      case _                    => 0.toLong
    }
  }
  
  def createCodeSimple(value:scala.Long, fixed:FixedType) (implicit writer:CodeWriter):SegmentReturn =  {
    
    val numberType   = if (hex) "h" else "d"
    val widthString  = if (fixed == FixedType.Simple || fixed.width == 0) "" else fixed.width.toString
    val prefix       = if (fixed.isSigned) "'s" + numberType else "'" + numberType // Select between hex and decimal
    val negative     = (value < 0)
    val numberString = if (hex)java.lang.Long.toHexString(math.abs(value).toLong) else math.abs(value).toLong.toString
    val negativeString = if (negative)  "-" else ""
    
    SegmentReturn(negativeString) + widthString + prefix + numberString
     
        
  }
  
  def createCode(implicit writer:CodeWriter) = createCodeSimple(this.longValue, this.fixed)
  
  
}

object NewConstant {
  


  /** 
   * Factory Methods to create a constant 
   **/
  def apply(value:Int):NewConstant = new Long(value.toLong, FixedType.Simple)
  def apply(value:Int,width:Int):NewConstant = new Long(value.toLong, FixedType.unsigned(width,0))
  def apply(value:scala.Long):NewConstant = new Long(value, FixedType.Simple)
  def apply(value:scala.Long,width:Int):NewConstant = new Long(value, FixedType.unsigned(width,0))
  def apply(value:Int,fixed:FixedType):NewConstant = new Long(value.toLong, fixed)
  def apply(value:scala.Long,fixed:FixedType):NewConstant = new Long(value, fixed)
  /** Double Methods */
  def apply(value:scala.Double):NewConstant = new Double(value,FixedType.Simple)
  def apply(value:scala.Double,fixed:FixedType):NewConstant = new Double(value,fixed)
  /** Maximum Value for this fixed type */
  def max(fixed:FixedType) = NewConstant((math.pow(2.0,fixed.width-1)-1)/math.pow(2.0,fixed.fraction),fixed)
  /** Minimum Value for this fixed type */
  def min(fixed:FixedType) = NewConstant(-(math.pow(2.0,fixed.width-1)-1)/math.pow(2.0,fixed.fraction),fixed)
 
  class Long(val value:scala.Long,  override val fixed:FixedType, override val outputType:OutputType = DECIMAL) extends NewConstant
  class Double(val value:scala.Double,  override val fixed:FixedType) extends NewConstant
  
  def Hex(value:scala.Long, width:Int) = new NewConstant.Long(value, FixedType.unsigned(width,0),HEX)
  def Octal(value:scala.Long, width:Int) = new NewConstant.Long(value, FixedType.unsigned(width,0),OCTAL)
  def Decimal(value:scala.Long, width:Int) = new NewConstant.Long(value, FixedType.unsigned(width,0),DECIMAL)


  class OutputType
  object DECIMAL extends OutputType
  object OCTAL extends OutputType
  object HEX extends OutputType
  
  
}