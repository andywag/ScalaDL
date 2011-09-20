package com.simplifide.generate.generator

import com.simplifide.generate.signal.FixedType
import com.simplifide.generate.parser.model.Expression
import com.simplifide.generate.parser.block.Statement
import com.simplifide.generate.blocks.basic.SimpleStatement
import com.simplifide.generate.parser.{SegmentHolder, ExpressionReturn}
import com.simplifide.generate.proc.Controls

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 5/29/11
 * Time: 4:26 PM
 * To change this template use File | Settings | File Templates.
 */

trait SimpleSegment extends Expression{

  val name = ""
  val fixed:FixedType = FixedType.Simple
  def numberOfChildren:Int = 0
  def child(index:Int):SimpleSegment = this
  /** Get a complete list of all children of this block */
  def children:List[SimpleSegment] = List.tabulate(numberOfChildren){x =>child(x)}
  /** All of the Children */
  def allChildren:List[SimpleSegment] = {
    if (numberOfChildren == 0)  List(this)
    else children.flatMap(x => x.allChildren)
  }
  /** Create an assignment based on this segment */
  def createAssign(output:SimpleSegment):SimpleSegment = new SimpleStatement.Assign(output,this)

  /** Return a sliced version of this segment */
  def sliceFixed(fixed:FixedType):SimpleSegment = this
  /** List of Extra Statements created from this statement */
  def extra:List[SimpleSegment] = List()

  def createCode(writer:CodeWriter):SegmentReturn

  def controlMatch(actual:SimpleSegment,statements:SegmentHolder):Boolean = false
  def createControl(actual:SimpleSegment,statements:SegmentHolder,index:Int):List[Controls] = List()


  def ++ (segment:SimpleSegment):SimpleSegment = new SimpleSegment.List(List(this,segment))
  def ++ (segment:String):SimpleSegment        = this ++ new SimpleSegment.Code(segment)

  override def split:List[Expression] = {
    return List(this)
  }

  //def split(output:Expression,index:Int):ExpressionReturn = new ExpressionReturn(this,List())



  /** Defines a true addition segment */
  //def + (segment:SimpleSegment):SimpleSegment



  /** Methods to create code segments */
  def createVerilogCode(writer:CodeWriter):SegmentReturn     = createCode(writer)
  def createVhdlCode(writer:CodeWriter):SegmentReturn        = createCode(writer)
  def createFloatCode(writer:CodeWriter):SegmentReturn       = createCode(writer)
  def createFixedCode(writer:CodeWriter):SegmentReturn       = createCode(writer)
  def createHeaderCode(writer:CodeWriter):SegmentReturn      = createCode(writer)
}

object SimpleSegment {

  def maxChildren(ch:scala.List[SimpleSegment]):Int = {
    ch.map(x => x.numberOfChildren).reduceLeft(math.max(_,_))
  }

  class Code(val value:String) extends SimpleSegment{
    def createCode(writer:CodeWriter):SegmentReturn =  new SegmentReturn(value,List())

  }

  class List(val segments:scala.List[SimpleSegment]) extends SimpleSegment{
    def createCode(writer:CodeWriter):SegmentReturn = {
       val segs = segments.map(writer.createCode(_))
       segs.reduceLeft(_+_)
    }

  }
}

