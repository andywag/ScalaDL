package com.simplifide.generate.parser.model

import com.simplifide.generate.parser.graph.Node
import com.simplifide.generate.parser.graph.Edge._

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/25/11
 * Time: 3:19 PM
 * To change this template use File | Settings | File Templates.
 */

trait Module extends Node {
  val name:String
  val signals:List[Signal]

  def -> (node:Module) = Instance("",this,node.asInstanceOf[Module],null)


  def inputs:List[Signal] = signals.filter(x => x.isInput)
  def output:List[Signal] = signals.filter(x => x.isOutput)

}

object Module {
  def apply(name:String, signals:List[Signal]) = new ModuleImpl(name,signals)
  class ModuleImpl(override val name:String, override val signals:List[Signal]) extends Module
}