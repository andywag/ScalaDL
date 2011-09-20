package com.simplifide.generate.hier

import com.simplifide.generate.parser.graph.Node
import com.simplifide.generate.signal.SignalTrait

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 8/12/11
 * Time: 5:42 PM
 * To change this template use File | Settings | File Templates.
 */

trait HierarchyModule extends Node {
  val name:String
  val signals:List[SignalTrait]

  //def createModule(instances:Option[List[HierarchyInstance]]):HierarchyModule


}

object HierarchyModule {

}