package com.simplifide.generate.parser

import block.Statement
import condition.Condition
import model._

import com.simplifide.generate.language.LanguageFactory
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.generator.SimpleSegment

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 7/16/11
 * Time: 9:38 AM
 * To change this template use File | Settings | File Templates.
 */

class ObjectFactory2 {}

object ObjectFactory2 {
  val factory = LanguageFactory
  //val factory = ParserFactory
  // Statements
  def Statement(output:Expression, input:Expression):SimpleSegment       = factory.Statement(output,input)
  def StatementReg(output:Expression, input:Expression):SimpleSegment    = factory.StatementReg(output,input)
  // Registers
  def Flop(clk:Clock,output:Expression,input:Expression):Expression = factory.Flop(clk,output,input)
  def Flop(clk:Clock,output:Expression,reset:Expression,input:Expression):Expression = factory.Flop(clk,output,reset,input)
  // Condition Statements
  def Question(condition:Expression, tru:Expression, fal:Expression) = factory.Question(condition,tru,fal)
  // Math Functions
  // Additions
  def Adder(lhs:Expression,rhs:Expression,negative:Boolean = false)                            = factory.Adder(lhs,rhs,negative)
  def AdderTrunc(lhs:Expression,rhs:Expression,negative:Boolean = false,fixed:Model.Fixed,internal:Model.Fixed)
    = factory.AdderTrunc(lhs,rhs,negative,fixed,internal)
  def AdderTruncClip(lhs:Expression,rhs:Expression,negative:Boolean = false,fixed:Model.Fixed, internal:Model.Fixed) =
    factory.AdderTruncClip(lhs,rhs,negative, fixed, internal)
  def AdderRound(lhs:Expression,rhs:Expression,negative:Boolean = false,fixed:Model.Fixed,internal:Model.Fixed)     =
    factory.AdderRound(lhs,rhs,negative,fixed,internal)
  def AdderRoundClip(lhs:Expression,rhs:Expression,negative:Boolean = false,fixed:Model.Fixed,internal:Model.Fixed) =
    factory.AdderRoundClip(lhs,rhs,negative,fixed,internal)
  // Multiplier
  def Mult(lhs:Expression,rhs:Expression)(implicit clk:ClockControl) =
    factory.Mult(lhs,rhs,Model.NoFixed,Model.NoFixed)(clk)
  def MultTrunc(lhs:Expression,rhs:Expression,fixed:Model.Fixed,internal:Model.Fixed)     =
    factory.MultTrunc(lhs,rhs,fixed,internal)
  def MultTruncClip(lhs:Expression,rhs:Expression,fixed:Model.Fixed,internal:Model.Fixed) =
    factory.MultTruncClip(lhs,rhs,fixed,internal)
  def MultRound(lhs:Expression,rhs:Expression,fixed:Model.Fixed,internal:Model.Fixed)     =
    factory.MultRound(lhs,rhs,fixed,internal)
  def MultRoundClip(lhs:Expression,rhs:Expression,fixed:Model.Fixed,internal:Model.Fixed) =
    factory.MultRoundClip(lhs,rhs,fixed,internal)
  // Division
  def Div(lhs:Expression,rhs:Expression)                            = factory.Div(lhs,rhs)
  def DivTrunc(lhs:Expression,rhs:Expression,fixed:Model.Fixed)     = factory.DivTrunc(lhs,rhs,fixed)
  def DivTruncClip(lhs:Expression,rhs:Expression,fixed:Model.Fixed) = factory.DivTruncClip(lhs,rhs,fixed)
  def DivRound(lhs:Expression,rhs:Expression,fixed:Model.Fixed)     = factory.DivRound(lhs,rhs,fixed)
  def DivRoundClip(lhs:Expression,rhs:Expression,fixed:Model.Fixed) = factory.DivRoundClip(lhs,rhs,fixed)
  // Rounding
  def Truncate(expression:Expression, fixed:Model.Fixed, internal:Model.Fixed)    =
    factory.Truncate(expression,fixed,internal)
  def TruncateClip(expression:Expression, fixed:Model.Fixed, internal:Model.Fixed) =
    factory.TruncateClip(expression,fixed,internal)
  def RoundInt(expression:Expression, fixed:Model.Fixed, internal:Model.Fixed)     =
    factory.RoundInt(expression,fixed,internal)
  def RoundClip(expression:Expression, fixed:Model.Fixed, internal:Model.Fixed)    =
    factory.RoundClip(expression,fixed,internal)
  // Relational Opeators
  def GT (lhs:Expression,rhs:Expression):Expression   = factory.GT(lhs,rhs)
  def LT (lhs:Expression,rhs:Expression):Expression   = factory.LT(lhs,rhs)
  def LTE (lhs:Expression,rhs:Expression):Expression  = factory.LTE(lhs,rhs)  // See <= Assign Statement
  def GTE (lhs:Expression,rhs:Expression):Expression  = factory.GTE(lhs,rhs)
  def EQ (lhs:Expression,rhs:Expression):Expression   = factory.EQ(lhs,rhs)
  def NEQ (lhs:Expression,rhs:Expression):Expression  = factory.NEQ(lhs,rhs)
  def EQ3 (lhs:Expression,rhs:Expression):Expression  = factory.EQ3(lhs,rhs)
  def NEQ3 (lhs:Expression,rhs:Expression):Expression = factory.NEQ3(lhs,rhs)
    // Logical and Reduction Operators
  def NOT (lhs:Expression,rhs:Expression):Expression  = factory.NOT(lhs,rhs)
  def AND (lhs:Expression,rhs:Expression):Expression  = factory.AND(lhs,rhs)
  def NAND (lhs:Expression,rhs:Expression):Expression = factory.NAND(lhs,rhs)
  def OR (lhs:Expression,rhs:Expression):Expression   = factory.OR(lhs,rhs)
  def NOR (lhs:Expression,rhs:Expression):Expression  = factory.NOR(lhs,rhs)
  def XOR (lhs:Expression,rhs:Expression):Expression  = factory.XOR(lhs,rhs)
  def NXOR (lhs:Expression,rhs:Expression):Expression = factory.NXOR(lhs,rhs)
    // Shift Operators
  def SL (lhs:Expression,rhs:Expression):Expression =   factory.SL(lhs,rhs)
  def SR (lhs:Expression,rhs:Expression):Expression =   factory.SR(lhs,rhs)
  // Condition
  def ConditionIf(statements:Expression)(values:List[Expression]):Condition =  factory.ConditionIf(statements)(values)
  def Case(condition:Expression)(statements:List[Expression]) = factory.Case(condition)(statements)
  //def CaseStatement(condition:Option[Expression],statement:Expression) = factory.CaseStatement(condition,statement)
  //
  def Always(values:List[Expression])(states:List[Expression]) = factory.Always(values)(states)
  def AlwaysStar(values:List[Expression]) = factory.AlwaysStar(values)

  // Signal
  def Signal(name:String, typ:SignalType = SignalType.SignalTypeImpl,fixed:Model.Fixed = Model.NoFixed)(arr:List[Int]) =
    factory.Signal(name, typ ,fixed)(arr.toList)

  def Constant(name:String = "",value:Double,fixed:Model.Fixed = Model.NoFixed) =
    factory.Constant(name,value,fixed)

}