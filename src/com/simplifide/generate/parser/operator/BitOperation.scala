package com.simplifide.generate.parser.operator

import com.simplifide.generate.parser.model.Expression


/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/15/11
 * Time: 5:38 PM
 * To change this template use File | Settings | File Templates.
 */

class BitOperation {

}

object BitOperations {


  class Concatenation(expressions:List[Expression]) extends Expression
  class Repeat(expression:Expression) extends Expression
}