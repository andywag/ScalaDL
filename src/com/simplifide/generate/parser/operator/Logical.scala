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


abstract class Logical(lhs:Expression, rhs:Expression) extends Operator(lhs,rhs)

object Logical {
    class NOT (lhs:Expression, rhs:Expression) extends Logical(lhs,rhs) {
      val operator:String = "~"
    }
    class AND (lhs:Expression, rhs:Expression) extends Logical(lhs,rhs)  {
      val operator:String = "&"
    }
    class NAND (lhs:Expression, rhs:Expression) extends Logical(lhs,rhs) {
      val operator:String = "~&"
    }
    class OR (lhs:Expression, rhs:Expression) extends Logical(lhs,rhs) {
      val operator:String = "|"
    }
    class NOR (lhs:Expression, rhs:Expression) extends Logical(lhs,rhs) {
      val operator:String = "~|"
    }
    class XOR (lhs:Expression, rhs:Expression) extends Logical(lhs,rhs) {
      val operator:String = "^"
    }
    class NXOR (lhs:Expression, rhs:Expression) extends Logical(lhs,rhs) {
      val operator:String = "~^"
    }


}