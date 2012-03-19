package com.simplifide.generate.parser.block

import com.simplifide.generate.generator.SimpleSegment
import com.simplifide.generate.blocks.basic.state.Always
import com.simplifide.generate.parser.items.{SingleCaseParser, SingleConditionParser, RegisterAtParser}
import com.simplifide.generate.blocks.basic.Statement
import com.simplifide.generate.signal.OpType
import com.simplifide.generate.parser.model.{EnclosedExpression, Expression}
import com.simplifide.generate.parser.factory.{HardwareFunctionCreationFactory, HardwareCreationFactory, CreationFactory}


/**
 * Expression which contains a generic statement of the form
 *
 * output  := input
 * output ::= input
 *
 */

trait ParserStatement extends Expression {

  /** Output of this statement */
  val output:Expression
  /** Input of the statement */
  val input:Expression


 def createHardware(implicit creator:CreationFactory):SimpleSegment = {
    val realOutput = output.create
    input match {
      case x:RegisterAtParser.Flop                    => input.createAssignment(realOutput)
      case x:EnclosedExpression                       => Always.Star(x.createAssignment(realOutput))  // Condition ParserStatement
      case _                                          => {
        realOutput.opType match {
          case OpType.Register        => new Statement.Reg(realOutput,input.createOutput(realOutput))
          case OpType.RegOutput => new Statement.Reg(realOutput,input.createOutput(realOutput))
          case _                      => new Statement.Assign(realOutput,input.createOutput(realOutput))
        }
      }
    }
  }

  def createHardwareFunction(implicit creator:CreationFactory):SimpleSegment = {
    val realOutput = output.create
    input match {
      case x:EnclosedExpression => input.createAssignment(realOutput)
      case _                    => new Statement.FunctionBody(realOutput,input.createOutput(realOutput))
    }

  }

  /** Create Flop Expressions */
  override def create(implicit creator:CreationFactory):SimpleSegment = {
    creator match {
      case HardwareCreationFactory         => createHardware
      case HardwareFunctionCreationFactory => createHardwareFunction
    }
  }

  override def createOutput(output:SimpleSegment)(implicit creator:CreationFactory) = create


  override def toString = "assign " + output + " = " + input

}

object ParserStatement {
  def apply(output:Expression, input:Expression) = new Impl(output,input)

  class Impl(override val output:Expression,override val input:Expression) extends ParserStatement {

  }
}