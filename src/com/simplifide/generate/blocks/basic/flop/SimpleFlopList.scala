package com.simplifide.generate.blocks.basic.flop

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import collection.mutable.{LinkedHashMap, ListBuffer}
import com.simplifide.generate.generator._
import com.simplifide.generate.blocks.basic.SimpleStatement
import scala.Some
import com.simplifide.generate.signal.{Constant, SignalTrait}
import com.simplifide.generate.parser.block.Statement
import com.simplifide.generate.parser.model.Expression
import com.simplifide.generate.parser.ExpressionReturn

/** Simple Flop */
class SimpleFlopList(val name1:Option[String],
                 val head:ClockControl,
                 val reset:List[SimpleFlopList.Segment],
                 val enable:List[SimpleFlopList.Segment]) extends SimpleSegment {
  
  /** Creates a reset list for this flop. */
  private val resetList:SimpleSegment = new BasicSegments.ListSegment(reset)

  /** Creates an enable list for this flop. */
  private val enableList:SimpleSegment = new BasicSegments.ListSegment(enable)


  override def split:List[SimpleSegment] = {
    val resets:List[SimpleFlopList.Segment]  = this.reset.flatMap(x => x.split)
    val enables:List[SimpleFlopList.Segment] = this.enable.flatMap(x => x.split)
    return List(new SimpleFlopList(this.name1,this.head,resets,enables))
  }


  override def createCode(writer:CodeWriter):SegmentReturn = {
    val flop = new ResetEnableFlop(name1,head,resetList,enableList)
    return writer.createCode(flop)
  }
  
  def createCCode(writer:CodeWriter):SegmentReturn = {
    val builder = new StringBuilder()
    for (en <- enable.reverse) {
        builder.append(writer.createCode(en.out))
        builder.append(" = ")
        builder.append(writer.createCode(en.in.get))
        builder.append(";\n")
    }
    return SegmentReturn.segment(builder.toString)
  }
  
  override def createFloatCode(writer:CodeWriter):SegmentReturn = {
    return createCCode(writer)
  }
  
  override def createFixedCode(writer:CodeWriter):SegmentReturn = {
    return createCCode(writer)
  }


}

object SimpleFlopList {
  /** Convenience class */
  class Segment(val out:SimpleSegment,val in:Option[SimpleSegment]) extends SimpleSegment {
    override def numberOfChildren:Int           = out.numberOfChildren
    override def child(index:Int):SimpleSegment = {
      new Segment(out.child(index), if (in == None) None else Some(in.get.child(index)))
    }

    override def split:List[SimpleFlopList.Segment] = {
      if (out.numberOfChildren > 0) {
        return this.in match {
          case Some(x) => (out.allChildren zip x.allChildren).map(x => new SimpleFlopList.Segment(x._1,Some(x._2)))
          case None    => out.allChildren.map(x => new SimpleFlopList.Segment(x,None))
        }
      }
      return List(this)
    }

    override def createCode(writer:CodeWriter):SegmentReturn = {
      if (this.numberOfChildren == 0) {
         val assign = this.in match {
            case Some(x) => new SimpleStatement.Reg(this.out,x)
            case None    => new SimpleStatement.Reg(this.out,Constant(0,this.out.fixed.width))
         }
         return writer.createCode(assign)
      }
      return SegmentReturn.combineFinalReturns(writer,allChildren,List())
    }
    //def allFlopChildren:List[SimpleFlopList.Segment] = this.allChildren.map(x => x.asInstanceOf[SimpleFlopList.Segment])

    def getResetSegment:Segment = new Segment(out,None)
  }

  /** Create a new simple flop based the list of inputs and outputs. The outputs are initialized to zero */
  def newFlop(clk:ClockControl,inputs:List[_ <: SignalTrait],outputs:List[SimpleSegment]):SimpleFlopList = {
    val res = inputs.map(x => new SimpleFlopList.Segment(x,None))
    val enas = new ListBuffer[SimpleFlopList.Segment]()
    for (i <- 0 until inputs.size) {
      enas.append(new SimpleFlopList.Segment(inputs(i),Some(outputs(i))))
    }
    new SimpleFlopList(None,clk,res,enas.toList)
  }
  /** Create a new simple flop based on a linked hashmap */
  def newFlop(clk:ClockControl,linkMap:LinkedHashMap[_ <: SignalTrait,_ <: SimpleSegment]):SimpleFlopList = {
    val res = linkMap.keys.map(x => new SimpleFlopList.Segment(x,None)).toList
    val enas = new ListBuffer[SimpleFlopList.Segment]()
    for ((key,value) <- linkMap) {
      enas.append(new SimpleFlopList.Segment(key,Some(value)))
    }
    new SimpleFlopList(None,clk,res,enas.toList)
  }

}



