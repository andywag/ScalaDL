package com.simplifide.generate.parser

import block.{Flop, Statement}
import condition.{Case, Condition, Question}
import math.{Division, Multiplier, Round}
import model._
import operator.{Shift, Logical, Comparison}

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 7/16/11
 * Time: 9:42 AM
 * To change this template use File | Settings | File Templates.
 */

/*
class ParserFactory {

}

object ParserFactory {
  def Statement(output:Expression, input:Expression):Statement       = com.simplifide.generate.parser.block.Statement(output,input)
  def StatementReg(output:Expression, input:Expression):Statement    = com.simplifide.generate.parser.block.Statement(output,input)
  def Flop(clk:Clock,output:List[Expression],input:List[Expression]) = new Flop(clk,output,None,input)
  def Flop(clk:Clock,output:List[Expression],reset:List[Expression],input:List[Expression]) = new Flop(clk,output,Some(reset),input)
  def Question(condition:Expression, tru:Expression, fal:Expression) = new Question(condition,tru,fal)
  // Math Function
  // Adders
  def Adder(lhs:Expression,rhs:Expression,negative:Boolean = false)
    = new com.simplifide.generate.parser.math.Adder("",lhs,rhs,negative)
  def AdderTrunc(lhs:Expression,rhs:Expression,negative:Boolean = false,fixed:Model.Fixed,internal:Option[Model.Fixed])
    = new com.simplifide.generate.parser.math.Adder.Truncation("",lhs,rhs,negative,fixed,internal)
  def AdderTruncClip(lhs:Expression,rhs:Expression,negative:Boolean = false,fixed:Model.Fixed,internal:Option[Model.Fixed])
    = new com.simplifide.generate.parser.math.Adder.TruncationClip("",lhs,rhs,negative,fixed,internal)
  def AdderRound(lhs:Expression,rhs:Expression,negative:Boolean = false,fixed:Model.Fixed,internal:Option[Model.Fixed])
    = new com.simplifide.generate.parser.math.Adder.Round("",lhs,rhs,negative,fixed,internal)
  def AdderRoundClip(lhs:Expression,rhs:Expression,negative:Boolean = false,fixed:Model.Fixed,internal:Option[Model.Fixed])
    = new com.simplifide.generate.parser.math.Adder.RoundClip("",lhs,rhs,negative,fixed,internal)
  // Rounding Operations
  def Truncate(expression:Expression, fixed:Model.Fixed)      = new Round.Truncate(expression,fixed)
  def TruncateClip(expression:Expression, fixed:Model.Fixed)  = new Round.TruncateClip(expression,fixed)
  def RoundInt(expression:Expression, fixed:Model.Fixed)      = new Round.RoundInt(expression,fixed)
  def RoundClip(expression:Expression, fixed:Model.Fixed)     = new Round.RoundClip(expression,fixed)
  // Multiplier Operation
  def Mult(lhs:Expression,rhs:Expression,negative:Boolean = false)                            = new Multiplier(lhs,rhs)
  def MultTrunc(lhs:Expression,rhs:Expression,fixed:Model.Fixed,negative:Boolean = false)     = new Multiplier.Truncation(lhs,rhs,fixed)
  def MultTruncClip(lhs:Expression,rhs:Expression,fixed:Model.Fixed,negative:Boolean = false) = new Multiplier.TruncationClip(lhs,rhs,fixed)
  def MultRound(lhs:Expression,rhs:Expression,fixed:Model.Fixed,negative:Boolean = false)     = new Multiplier.Round(lhs,rhs,fixed)
  def MultRoundClip(lhs:Expression,rhs:Expression,fixed:Model.Fixed,negative:Boolean = false) = new Multiplier.RoundClip(lhs,rhs,fixed)
  // Division Operation
  def Div(lhs:Expression,rhs:Expression,negative:Boolean = false)                            = new Division(lhs,rhs)
  def DivTrunc(lhs:Expression,rhs:Expression,fixed:Model.Fixed,negative:Boolean = false)     = new Division.Truncation(lhs,rhs,fixed)
  def DivTruncClip(lhs:Expression,rhs:Expression,fixed:Model.Fixed,negative:Boolean = false) = new Division.TruncationClip(lhs,rhs,fixed)
  def DivRound(lhs:Expression,rhs:Expression,fixed:Model.Fixed,negative:Boolean = false)     = new Division.Round(lhs,rhs,fixed)
  def DivRoundClip(lhs:Expression,rhs:Expression,fixed:Model.Fixed,negative:Boolean = false) = new Division.RoundClip(lhs,rhs,fixed)
  // Comparison Operators
  def GT (lhs:Expression,rhs:Expression):Expression   = new Comparison.GT(lhs,rhs)
  def LT (lhs:Expression,rhs:Expression):Expression   = new Comparison.LT(lhs,rhs)
  def LTE (lhs:Expression,rhs:Expression):Expression  = new Comparison.LTE(lhs,rhs)  // See <= Assign Statement
  def GTE (lhs:Expression,rhs:Expression):Expression  = new Comparison.GTE(lhs,rhs)
  def EQ (lhs:Expression,rhs:Expression):Expression   = new Comparison.EQ(lhs,rhs)
  def NEQ (lhs:Expression,rhs:Expression):Expression  = new Comparison.NEQ(lhs,rhs)
  def EQ3 (lhs:Expression,rhs:Expression):Expression  = new Comparison.EQ3(lhs,rhs)
  def NEQ3 (lhs:Expression,rhs:Expression):Expression = new Comparison.NEQ3(lhs,rhs)
  // Logical and Reduction Operators
  def NOT (lhs:Expression,rhs:Expression):Expression  = new Logical.NOT(lhs,rhs)
  def AND (lhs:Expression,rhs:Expression):Expression  = new Logical.AND(lhs,rhs)
  def NAND (lhs:Expression,rhs:Expression):Expression = new Logical.NAND(lhs,rhs)
  def OR (lhs:Expression,rhs:Expression):Expression  = new Logical.OR(lhs,rhs)
  def NOR (lhs:Expression,rhs:Expression):Expression  = new Logical.NOR(lhs,rhs)
  def XOR (lhs:Expression,rhs:Expression):Expression  = new Logical.XOR(lhs,rhs)
  def NXOR (lhs:Expression,rhs:Expression):Expression = new Logical.NXOR(lhs,rhs)
  // Shift Operators
  def SL (lhs:Expression,rhs:Expression):Expression   = new Shift.SL(lhs,rhs)
  def SR (lhs:Expression,rhs:Expression):Expression   = new Shift.SR(lhs,rhs)
  // Signal Creation
  def Signal(name:String, typ:SignalType = SignalType.SignalTypeImpl,fixed:Model.Fixed = Model.Fixed(1,0))(arr:List[Int]) =
    com.simplifide.generate.parser.model.Signal(name)
  // Condition
  def ConditionIf(condition:Expression)(values:List[Expression]):Condition = Condition(condition,values)
  // Cae Statement
  def Case(condition:Expression)(statements:List[Expression]) = com.simplifide.generate.parser.condition.Case(condition,statements)
  def Always(values:List[Expression])(states:List[Expression]) = null
  def AlwaysStar(values:List[Expression])                      = null

}
*/