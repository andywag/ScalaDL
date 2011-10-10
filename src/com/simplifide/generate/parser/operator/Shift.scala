package com.simplifide.generate.parser.operator

import com.simplifide.generate.parser.{ObjectFactory}
import com.simplifide.generate.parser.model.Expression

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/19/11
 * Time: 3:32 PM
 * To change this template use File | Settings | File Templates.
 */


abstract class Shift(lhs:Expression, rhs:Expression) extends Operator(lhs,rhs)

object Shift {

    class SL (lhs:Expression, rhs:Expression) extends Shift(lhs,rhs) {
      val operator:String = "<<"
    }
    class SR (lhs:Expression, rhs:Expression) extends Shift(lhs,rhs)  {
      val operator:String = ">>"
    }

}