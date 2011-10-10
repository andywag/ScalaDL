package com.simplifide.generate.parser.graph

import com.simplifide.generate.parser.model.Expression


/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/22/11
 * Time: 1:53 PM
 * To change this template use File | Settings | File Templates.
 */

trait Edge[M <: Node] {
  val source:M
  val destination:M
  val connection:Option[Connection]

  //def ## (values:Any) = Edge(this.source, this.destination)

}

object Edge {
   def apply[N <: Node](source:N, destination:N,connection:Option[Connection]) =
     new EdgeImpl[N](source,destination,connection)

   class EdgeImpl[N <: Node](override val source:N,
                             override val destination:N,
                             override val connection:Option[Connection]) extends Edge[N]
}