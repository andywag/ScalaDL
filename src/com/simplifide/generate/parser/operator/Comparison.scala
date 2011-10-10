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

abstract class Comparison(lhs:Expression, rhs:Expression) extends Operator(lhs,rhs)

object Comparison {
    class LT (lhs:Expression, rhs:Expression) extends Comparison(lhs,rhs) {
      val operator:String = "<"
    }
    class GT (lhs:Expression, rhs:Expression) extends Comparison(lhs,rhs)  {
      val operator:String = ">"
    }
    class LTE (lhs:Expression, rhs:Expression) extends Comparison(lhs,rhs) {
      val operator:String = "<="
    }
    class GTE (lhs:Expression, rhs:Expression) extends Comparison(lhs,rhs) {
      val operator:String = ">="
    }
    class EQ (lhs:Expression, rhs:Expression) extends Comparison(lhs,rhs) {
      val operator:String = "=="
    }
    class NEQ (lhs:Expression, rhs:Expression) extends Comparison(lhs,rhs) {
      val operator:String = "!="
    }
    class EQ3 (lhs:Expression, rhs:Expression) extends Comparison(lhs,rhs) {
       val operator:String = "==="
    }
    class NEQ3 (lhs:Expression, rhs:Expression) extends Comparison(lhs,rhs) {
       val operator:String = "!=="
    }

}