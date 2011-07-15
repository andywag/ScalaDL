package com.simplifide.generate.parser

import collection.mutable.ListBuffer

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/14/11
 * Time: 2:57 PM
 * To change this template use File | Settings | File Templates.
 */

class BaseParser {
  val statements = new ListBuffer[Expression]()

  private def debugState(statement:Expression) {
    System.out.println(statement)
    statement.split.foreach(x => System.out.println("   " + x))
  }
  def debug {
    statements.foreach(System.out.println(_))
    System.out.println("Debugging Split")
    statements.foreach(x => debugState(x))
  }

  def assign(statement:Statement) = statements.append(statement)




}

object BaseParser {

}