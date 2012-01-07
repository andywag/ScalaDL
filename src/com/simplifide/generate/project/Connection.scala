package com.simplifide.generate.project

import com.simplifide.generate.signal.SignalTrait


/** Connection Class which converts signal names for instantiation */
class Connection {
  def connect(signal:SignalTrait):SignalTrait = signal
}

object Connection {

  object Default extends Connection

  /**
   * Class which defines a connection based on a map of strings which converts a signal based on name. If a signal name
   * isn't found the signal's name remains the same
   *
   * @constructor
   * @parameter connections Map of conversions
   */
  class MapConnection(connections:Map[String,String]) extends Connection {
    override def connect(signal:SignalTrait):SignalTrait = {
      connections.get(signal.name) match {
        case Some(x) => return signal.copy(x)
        case None    => return signal
      }
    }
  }

  class MapSignalConnection(connections:Map[SignalTrait,SignalTrait]) extends Connection {
    override def connect(signal:SignalTrait):SignalTrait = connections.getOrElse(signal,signal)
  }
  object MapSignalConnection {
    /*def apply(connections:(SignalTrait,SignalTrait)*):MapSignalConnection = {
      MapSignalConnection(Map(connections))
    }*/
    def apply(connections:Map[SignalTrait,SignalTrait]):MapSignalConnection = {
      val imap = connections.flatMap(x => x._1.connect(x._2))
      new MapSignalConnection(imap)
    }
  }

}