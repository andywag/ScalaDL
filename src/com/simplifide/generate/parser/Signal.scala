package com.simplifide.generate.parser

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/12/11
 * Time: 3:48 PM
 * To change this template use File | Settings | File Templates.
 */

class Signal(val name:String, val scope:SignalParser) extends Expression {
    override def toString = name
    def apply(clk:Model.Clock) = new Signal.Delay(this,clk)
    def apply(top:Int,bot:Int) = new Signal(this.name,scope)

    def copy(index:Int):Signal = new Signal(name + "_" + index,scope)

    def <= (rhs:Expression):Statement = {
      val state = new Statement(this,rhs)
      scope.assign(state)
      state
    }

    def := (rhs:Expression):Statement = {
      new Statement(this,rhs)
    }

        // Unary Operators
    override def unary_- : Expression = new Adder.NegativeTerm(this)

}

object Signal {
  class Delay(signal:Signal, clk:Model.Clock) extends Signal(signal.name, signal.scope) {
    override def toString = signal.name + "[" + clk + "]"
  }
}