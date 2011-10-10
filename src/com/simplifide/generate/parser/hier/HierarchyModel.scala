package com.simplifide.generate.parser.hier

import com.simplifide.generate.parser.block.state.State.Transition
import com.simplifide.generate.parser.graph.{Edge, Node, Graph}
import com.simplifide.generate.parser.model.{Instance, Module}

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/25/11
 * Time: 5:27 PM
 * To change this template use File | Settings | File Templates.
 */

class HierarchyModel(edges:List[Instance]) extends Graph[Module,Instance](edges) {
   //def group:Map[Impl,List[Transition]] = edges.groupBy(x => x.source)

}

object HierarchyModel {
  def apply(edges:Instance*) = new HierarchyModel(edges.toList)

}