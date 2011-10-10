package com.simplifide.generate.parser.block.state

import com.simplifide.generate.parser.graph.Edge.EdgeImpl
import com.simplifide.generate.parser.graph.{Edge, Graph, Node}
import com.simplifide.generate.parser.model.{Signal, Expression}
import com.simplifide.generate.generator.SimpleSegment
import com.simplifide.generate.html.Description

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/21/11
 * Time: 3:54 PM
 * To change this template use File | Settings | File Templates.
 */

class State(val name:String,
            val index:Int,
            val expressions:List[SimpleSegment] = List(),
            val description:Option[Description] = None)  extends Node {

    def -> (node:State) = new State.Transition(this,node,None)
    def -- (description:Description) = new State(name,index,expressions,Some(description))
}

object State {

  def apply(name:String, index:Int, segments:List[SimpleSegment] = List()) =
    new State(name,index,segments)

  class Transition(override val source:State,
                   override val destination:State,
                   val expr:Option[SimpleSegment],
                   val comment:Option[String] = None) extends Edge[State] {

    override val connection = null

    def ##(expression:SimpleSegment) = new Transition(source, destination, Some(expression))
    def --(comment:String) = new Transition(source,destination,expr,Some(comment))
  }

  /*def main (args:Array[String]) = {
    val (st0,st1)  = (new State("a",0), new State("b",1))

    val gr = Graph((st0 -> st1) ## Signal("alpha"))
  }*/
}