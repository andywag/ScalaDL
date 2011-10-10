package com.simplifide.generate.parser

import com.simplifide.generate.language.ExtraFile
import collection.mutable.ListBuffer

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/21/11
 * Time: 11:42 AM
 * To change this template use File | Settings | File Templates.
 */

class ModuleParser extends ConditionParser with SignalParser {

  val extraFiles = new ListBuffer[ExtraFile]()
  /** Splits the statements into groups */
  def transform = {
    val newStatements = this.allStatements.flatMap(x => if (x != null) x.split else List())
    this.statements.clear
    this.statements.appendAll(newStatements)
  }

}