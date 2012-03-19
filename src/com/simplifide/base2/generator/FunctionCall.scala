package com.simplifide.base2.generator

/**
 */

trait FunctionCall extends ScalaObject{

  val functionName:String
  val functionItems:List[String]


  def generate:String = {
    val body = functionItems.zipWithIndex.map(x => (if (x._2 != 0) "," else "") + x._1).foldLeft("")(_+_)
    functionName + "(" + body + ")"
  }
  
  
}

object FunctionCall {
  def apply(functionName:String,functionItems:List[String]) = new Impl(functionName,functionItems)

  class Impl(val functionName:String,val functionItems:List[String]) extends FunctionCall
}