package com.simplifide.generate.blocks.basic.operator

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import com.simplifide.generate.signal.{SignalTrait, Signing, FixedType}
import com.simplifide.generate.generator.{SimpleSegment, CodeWriter, SegmentReturn}

class Select(val signal:SimpleSegment,
             val top:Option[Int],
             val bot:Option[Int],
             val floor:Int) extends SimpleSegment {

  def this(signal:SimpleSegment,top:Option[Int], bot:Option[Int]) {
    this(signal,top,bot,0)
  }
	
  override val fixed:FixedType = {
	  top match {
	 	  case None    => new FixedType.Main(Signing.UnSigned,1,0)
	 	  case Some(x) => bot match {
	 	 	  case None    => new FixedType.Main(Signing.UnSigned,1,0)
	 	 	  case Some(y) => new FixedType.Main(Signing.UnSigned,x-y,0)
        case _     =>  new FixedType.Main(Signing.UnSigned,1,0)
	 	  }
	  }

  }
  override def createCode(writer:CodeWriter):SegmentReturn = {
    createVerilogCode(writer)
  }

  override def createVerilogCode(writer:CodeWriter):SegmentReturn = {
    
    top match {
      case None => return writer.createCode(signal)// No Selection 
      case Some(x) => bot match {
          case None    => {
           val builder = new StringBuilder();
           builder.append(writer.createCode(signal).code)
           builder.append("[")
           builder.append(x.toString)
           builder.append("]")
           return SegmentReturn.segment(builder.toString)
          }
          case Some(y) => 
            val builder = new StringBuilder();
            if (x < signal.fixed.width) { // No Sign Extension
               if (y < floor) { // Zero Pad
                 builder.append("{")
                 builder.append(writer.createCode(signal).code)
                 builder.append("[")
                 builder.append(x.toString)
                 builder.append(":"); builder.append(floor); builder.append("]")

                 builder.append(",")
                 builder.append((floor-y).toString)
                 builder.append("'d0")
                 builder.append("}")
               }
               else if (y >= 0) { // Truncate
                  builder.append(writer.createCode(signal).code)
                  builder.append("[")
                  builder.append(x.toString)
                  builder.append(":")
                  builder.append(y.toString)
                  builder.append("]")
               }
            }
            else { // Sign Extension
              builder.append("{")
              if (signal.fixed.signed.isSigned) { // Signed Solution
                builder.append("{")
                builder.append((x-signal.fixed.width+1).toString)
                builder.append("{")
                builder.append(signal.name)
                builder.append("[")
                builder.append((signal.fixed.width-1).toString)
                builder.append("]")
                builder.append("}")
                builder.append("}")
              }
              else { // Unsigned Solution
                builder.append( (x-signal.fixed.width).toString)
                builder.append("'d0");
              }
              builder.append(",")
              builder.append(writer.createCode(signal).code)
              if (y < 0) { // Zero Pad
                 builder.append(",")
                 builder.append((-y).toString)
                 builder.append("'d0")
               }
               else if (y > 0) { // Truncate
                  //builder.append(appendSignal.createVerilogCode(context).code)
                  builder.append("[")
                  builder.append((signal.fixed.width-1).toString)
                  builder.append(":")
                  builder.append(y.toString)
                  builder.append("]")
               }
              builder.append("}")
            }
            SegmentReturn.segment(builder.toString)
            
      }
    }
    
  }
  
}

object Select {

  def apply(state:SignalTrait,top:Int,bot:Int):Select = new Select(state,Some(top),Some(bot))


  def sign(state:SignalTrait) = newSelect(state,state.fixed.width-1)
  def newSelect(state:SignalTrait,top:Int):Select = new Select(state,Some(top),None)
  def newSelect(state:SignalTrait,top:Int,bot:Int):Select = new Select(state,Some(top),Some(bot))


}

