package com.simplifide.generate.parser.operator

import com.simplifide.generate.parser.model.Expression
import com.simplifide.generate.generator.SimpleSegment
import com.simplifide.generate.parser.factory.CreationFactory


/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/15/11
 * Time: 5:38 PM
 * To change this template use File | Settings | File Templates.
 */

class BitOperation {

}

// TODO Fix these classes
object BitOperations {


  class Concatenation(expressions:List[Expression]) extends Expression {
    /** Create the simple segment */
    def create(implicit creator:CreationFactory):SimpleSegment = null
    /** Create Expression as a function of the output */
    def createOutput(output:SimpleSegment)(implicit creator:CreationFactory):SimpleSegment = null
  }

  class Repeat(expression:Expression) extends Expression {
    /** Create the simple segment */
    def create(implicit creator:CreationFactory):SimpleSegment = null
    /** Create Expression as a function of the output */
    def createOutput(output:SimpleSegment)(implicit creator:CreationFactory):SimpleSegment = null
  }
}