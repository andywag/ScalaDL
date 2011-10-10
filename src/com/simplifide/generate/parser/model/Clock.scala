package com.simplifide.generate.parser.model



/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/15/11
 * Time: 5:09 PM
 * To change this template use File | Settings | File Templates.
 */

trait Clock extends Expression {
     /** Name of the clock */
     val name:String
     /** Delay of the clock when evaluated */
     val delay:Int
     // TODO More generic handling of time differences
     def -(rhs:Int) = Clock(name, rhs)

     override def toString = name + (if (delay != 0) "-" + delay else "")

}

object Clock {
  def apply(name:String, delay:Int = 0) = new Implementation(name,delay)

  class  Implementation(override val name:String, override val delay:Int = 0) extends Clock
}