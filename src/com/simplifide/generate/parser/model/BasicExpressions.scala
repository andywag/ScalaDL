package com.simplifide.generate.parser.model

import com.simplifide.generate.generator.{BasicSegments, SimpleSegment}


/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 12/19/11
 * Time: 1:03 PM
 * To change this template use File | Settings | File Templates.
 */

class BasicExpressions {

}

object BasicExpressions {

  def List(terms:scala.List[Expression]):Expression = if (terms.length == 1) terms(0) else new List(terms)
  
  class List(val terms:scala.List[Expression]) extends Expression {
    override def create = BasicSegments.List(terms.map(_.create))

  }
}