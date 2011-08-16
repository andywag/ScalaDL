package com.simplifide.generate.parser

import block.Statement
import model.Expression
import com.simplifide.generate.generator.SimpleSegment

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/12/11
 * Time: 4:39 PM
 * To change this template use File | Settings | File Templates.
 */

class ExpressionReturn(val output:Expression, val states:List[SimpleSegment]) {

  val segmentStates = states.map(_.asInstanceOf[SimpleSegment])
}