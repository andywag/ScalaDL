package com.simplifide.generate.blocks.basic

import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, SimpleSegment}
import com.simplifide.generate.generator.SegmentReturn._
import com.simplifide.generate.parser.block.Statement
import com.simplifide.generate.parser.ExpressionReturn
import com.simplifide.generate.parser.model.Expression

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 6/11/11
 * Time: 10:50 AM
 * To change this template use File | Settings | File Templates.
 */

abstract class SimpleStatement(val output:SimpleSegment, val input:SimpleSegment) extends SimpleSegment with Statement {
  def newAssignment(output:SimpleSegment,input:SimpleSegment):SimpleStatement

}

object SimpleStatement {

  class Assign(output:SimpleSegment,input:SimpleSegment) extends SimpleStatement(output,input) {

    def newAssignment(output:SimpleSegment,input:SimpleSegment) = new Assign(output,input)

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
      def handleExpression(seg:Expression,expr:ExpressionReturn) = {
        if (expr.states.size == 0) List(seg.asInstanceOf[SimpleStatement]) else expr.states.map(_.asInstanceOf[SimpleStatement])
      }
      val out =  busSplit.map(x => (x,x.input.split(x.output,-1)))
      val rout = out.flatMap(x => handleExpression(x._1,x._2))
      rout
    }

    override def createCode(writer:CodeWriter):SegmentReturn = {
      // If No Children Create a single Item
      //if (output.numberOfChildren == 0) {
        val inC  = writer.createCode(input)
        val outC = writer.createCode(output)
        val ext = inC.extra.map(x => writer.createCode(x))
        val ret = returnSegment(outC,inC)
        val segments = ext ::: List(ret)
        return segments.reduceLeft(_ +_)
      //}
      // Create a List of the Rest of the Statements
      /*val outC = output.allChildren;
      val inC  = input.allChildren
      val outIn:List[(SimpleSegment,SimpleSegment)] = (outC zip inC)
      val segments:List[SimpleSegment] = outIn.map(x => newAssignment(x._1,x._2))
      return SegmentReturn.combineListReturns(writer,segments)
      */
    }
  }

  class Reg(output:SimpleSegment,input:SimpleSegment) extends Assign(output,input) {

    override def newAssignment(output:SimpleSegment,input:SimpleSegment) = new Reg(output,input)

    override def returnSegment(outSegment:SegmentReturn,inSegment:SegmentReturn):SegmentReturn =
       outSegment + " <= " + inSegment + ";\n"
  }



}