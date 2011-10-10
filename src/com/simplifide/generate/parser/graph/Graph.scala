package com.simplifide.generate.parser.graph

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/22/11
 * Time: 1:50 PM
 * To change this template use File | Settings | File Templates.
 */

class Graph[M <: Node,N <: Edge[M]](val edges:List[N]) {




  val nodes:List[M] = {
    val allNodes = edges.map(_.source) ::: edges.map(_.destination)
    allNodes.distinct
  }

}

object Graph {
  def apply[M <: Node](edges:Edge[M]*) = new Graph[M,Edge[M]](edges.toList)
}