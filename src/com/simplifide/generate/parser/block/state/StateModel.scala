package com.simplifide.generate.parser.block.state

import com.simplifide.generate.parser.block.state.State.Transition
import com.simplifide.generate.parser.graph.{Edge, Node, Graph}
import com.simplifide.generate.parser.model.Module

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/25/11
 * Time: 5:27 PM
 * To change this template use File | Settings | File Templates.
 */

class StateModel(edges:List[Transition]) extends Graph[State,Transition](edges) {
   def groups:Map[State,List[Transition]] = edges.groupBy(x => x.source)

}

object StateModel {
  def apply(edges:Transition*) = new StateModel(edges.toList)

}