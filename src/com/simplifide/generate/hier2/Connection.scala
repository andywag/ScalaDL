package com.simplifide.generate.hier2

import com.simplifide.generate.signal.SignalTrait

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 9/8/11
 * Time: 4:12 PM
 * To change this template use File | Settings | File Templates.
 */

/** Connection Class which converts signal names for instantiation */
class Connection {
  def connect(signal:SignalTrait):SignalTrait = signal
}

object Connection {

  object Default extends Connection

  class MapConnection(connections:Map[String,String]) extends Connection {
    override def connect(signal:SignalTrait):SignalTrait = {
      connections.get(signal.name) match {
        case Some(x) => return signal.copy(signal.name)
        case None    => return signal
      }
    }

  }
}