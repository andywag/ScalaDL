package com.simplifide.generate.blocks.test

import com.simplifide.generate.generator.SimpleSegment
import com.simplifide.generate.parser.model.Expression
import com.simplifide.generate.blocks.basic.misc.Lut
import com.simplifide.generate.blocks.basic.SimpleStatement
import com.simplifide.generate.blocks.basic.operator.{BinaryOperator, AbsoluteValue}
import com.simplifide.generate.signal.{Constant, OpType, SignalTrait}

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 9/29/11
 * Time: 6:10 PM
 * To change this template use File | Settings | File Templates.
 */

class CompareTable(val output:SignalTrait,
  val condition:SimpleSegment,
  val values:List[SimpleSegment],
  val start:Int,
  val finish:Int,
  val threshold:Int = 0) extends SimpleSegment.Combo {

  override def split:List[Expression] = {
    val cop = output.copy(output.name + "_comp",OpType.Register)
    val err = output.copy(output.name + "_err", OpType.Signal)
    val abs = output.copy(output.name + "_abs", OpType.Signal)
    val ind = SignalTrait(output.name + "_ind", OpType.Signal)

    val lut        = Lut(cop,condition,values).split
    val delta      = new SimpleStatement.Assign(err,(output - cop).asInstanceOf[SimpleSegment],List(cop,err,abs,ind))
    val absValue   = new SimpleStatement.Assign(abs,new AbsoluteValue(err))
    val absInd     = new SimpleStatement.Assign(ind, BinaryOperator.GT(abs,Constant(threshold,abs.fixed.width)))
    lut ::: List(delta,absValue,absInd)
  }

}

object CompareTable {

  def apply(output:SignalTrait,
    condition:SimpleSegment,
    values:List[SimpleSegment],
    start:Int,
    finish:Int,
    threshold:Int = 0) = new CompareTable(output,condition,values,start,finish,threshold)

  def apply(output:SignalTrait,
    condition:SimpleSegment,
    creator:Int=>SimpleSegment,
    start:Int,
    finish:Int) = new CompareTable(output,condition,List.tabulate(finish)(creator(_)),start,finish,0)

}