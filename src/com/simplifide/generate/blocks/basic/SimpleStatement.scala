package com.simplifide.generate.blocks.basic

import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, SimpleSegment}
import com.simplifide.generate.generator.SegmentReturn._
import com.simplifide.generate.parser.block.Statement
import com.simplifide.generate.parser.model.Expression
import com.simplifide.generate.signal.SignalTrait
import com.simplifide.generate.proc.Controls
import com.simplifide.generate.parser.{SegmentHolder, ExpressionReturn}


/**
 * Assignment Statement
 *
 * @constructor
 * @parameter output Output of the Statement
 * @parameter input Input of the Statement
 * @parameter extraSignals Extra signals which can be attached to the statement to be added to the module later
 */

abstract class SimpleStatement(val output:SimpleSegment,
                               val input:SimpleSegment,
                               val extraSignals:List[SignalTrait] = List()) extends SimpleSegment with Statement {

  /** Create a new Assignment */
  protected def newAssignment(output:SimpleSegment,input:SimpleSegment,extra:List[SignalTrait] = List()):SimpleStatement

  // TODO Needs to be moved to another location
  /** Controls for Processor Generator */
  override def controls = input.controls
  /** Control Generation for the Processor Generator */
  def createControl(actual:SimpleStatement,statements:SegmentHolder, index:Int):List[Controls] = {
    this.input.createControl(actual.input,statements,index)
  }

  def returnSegmentReg(outSegment:SegmentReturn,inSegment:SegmentReturn):SegmentReturn =
    outSegment + " <= " + inSegment + ";\n"

  def returnSegment(outSegment:SegmentReturn,inSegment:SegmentReturn):SegmentReturn =
    SegmentReturn.segment("assign ") + outSegment + " = " + inSegment + ";\n"


   /** Splits this statement into a group of statements. This has a multi pass structure. The first pass consists of
     *  1. Splitting this up based on the array or bus type
     *  2. Potentially splitting this into multiple statements if required
     * */
    override def split:List[SimpleSegment] = {
      def busSplit:List[Statement] = {
        if (output.numberOfChildren > 0) {
          val outputChildren = output.allChildren
          val inputChildren  = input.allChildren
          return (outputChildren zip inputChildren).map(x => newAssignment(x._1,x._2))
        }
        List(this)
      }
      def handleExpression(seg:SimpleStatement,expr:ExpressionReturn) = {
        if (expr.states.size == 0) List(seg.newAssignment(seg.output,expr.output.asInstanceOf[SimpleSegment],seg.extraSignals ::: expr.signals))
        else expr.states.map(_.asInstanceOf[SimpleSegment])
      }
      val out =  busSplit.map(x => (x,x.input.split(x.output,-1)))
      val rout = out.flatMap(x => handleExpression(x._1.asInstanceOf[SimpleStatement],x._2))
      rout
    }

    override def createCode(writer:CodeWriter):SegmentReturn = {
      val inC  = writer.createCode(input)
      val outC = writer.createCode(output)

      val ret = {
        if (output.isInstanceOf[SignalTrait] && output.asInstanceOf[SignalTrait].opType.isReg)
         returnSegmentReg(outC,inC)
        else returnSegment(outC,inC)
      }

      new SegmentReturn(ret.code,List(),inC.extra,inC.internal ::: extraSignals)

    }


}

/** Methods and classes for creating statements */
object SimpleStatement {


  /** Assign Statement used for a Wire */
  class Assign(output:SimpleSegment,
               input:SimpleSegment,
               extra:List[SignalTrait] = List()) extends SimpleStatement(output,input,extra) {

    def newAssignment(output:SimpleSegment,input:SimpleSegment, extra:List[SignalTrait] = List()) =
      new Assign(output,input,extra)




  }


  /** Statement Used inside Always Block */
  class Reg(output:SimpleSegment,
            input:SimpleSegment,
            extra:List[SignalTrait] = List()) extends SimpleStatement(output,input,extra) {

   override def newAssignment(output:SimpleSegment,input:SimpleSegment, extra:List[SignalTrait] = List()) =
      new Reg(output,input,extra)

    override def returnSegment(outSegment:SegmentReturn,inSegment:SegmentReturn):SegmentReturn =
       outSegment + " <= " + inSegment + ";\n"
  }

  class Body(output:SimpleSegment,
             input:SimpleSegment,
             extra:List[SignalTrait] = List()) extends SimpleStatement(output,input,extra) {

   override def newAssignment(output:SimpleSegment,input:SimpleSegment, extra:List[SignalTrait] = List()) =
      new Reg(output,input,extra)

    override def returnSegment(outSegment:SegmentReturn,inSegment:SegmentReturn):SegmentReturn =
       outSegment + " = " + inSegment + ";\n"
  }



}