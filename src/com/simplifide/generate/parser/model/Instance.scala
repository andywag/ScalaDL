package com.simplifide.generate.parser.model

import com.simplifide.generate.parser.graph.{Connection, Edge}

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/25/11
 * Time: 3:18 PM
 * To change this template use File | Settings | File Templates.
 */

trait Instance extends Edge[Module]{
  val name:String

  //val source:Impl
  //val destination:Impl
  //val connection:Option[Connection]
}

object Instance {

  def apply(name:String, source:Module, destination:Module, connection:Option[Instance.Connection] = None) =
    new InstanceImpl(name,source,destination,connection)

  class InstanceImpl(override val name:String,
                     override val source:Module,
                     override val destination:Module,
                     override val connection:Option[Connection]) extends Instance


  trait Connection extends com.simplifide.generate.parser.graph.Connection {

  }


}