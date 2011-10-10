package com.simplifide.generate.parser.graph

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/22/11
 * Time: 1:54 PM
 * To change this template use File | Settings | File Templates.
 */

trait Node {
  def -> (node:Node) = Edge(this,node,None)


}