package com.simplifide.generate.project

import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, SimpleSegment}
import com.simplifide.generate.signal.SignalTrait


/**
 * Segment which is used to create a module instantiation based on the child Entity
 */
trait NewEntityInstance[T <: NewEntity] extends SimpleSegment{

  val entity:T
  val connection:Connection = Connection.Default

  import entity._

  override def toString = name + "(" + entity + ")"

  def allSignals = SignalTrait.uniqueSignals(entity.signals.flatMap(_.allSignalChildren).filter(_.isIo))
  /** Returns a list of all signals as seen at the enclosing module */
  //def allSignals = entity.signals.flatMap(_.allSignalChildren).map(connection.connect(_))
  // TODO Need to add signal name conversion
  override def createCode(implicit writer:CodeWriter):SegmentReturn = {
    def createSignals:SegmentReturn = {
      def createSignal(signal:SignalTrait, index:Int):SegmentReturn =
        (if (index != 0) ",\n    " else "\n    ") +  "." + signal.name + "(" + connection.connect(signal).name +")"
      allSignals.zipWithIndex.map(x => createSignal(x._1,x._2)).foldLeft(SegmentReturn(""))(_ + _)
    }
    val out = SegmentReturn(entity.name) + " " + this.name + " (" + createSignals + ");\n\n"
    out
  }



  /** Pass which connects the inputs to the entity instances */
  def inputPass:NewEntityInstance[NewEntity]  =
    NewEntityInstance(this.entity.inputPass,this.name,this.connection)

  /** Pass which connects the outputs to the entity instances */
  def outputPass(outputs:Option[List[SignalTrait]]):NewEntityInstance[NewEntity] =
    NewEntityInstance(this.entity.outputPass(outputs),this.name,this.connection)

}

/** Factory method for creating an instance */
object NewEntityInstance {
  def apply[T <: NewEntity](entity:T) = new Impl(entity,entity.name,Connection.Default)
  def apply[T <: NewEntity](entity:T, name:String) = new Impl(entity,name,Connection.Default)
  def apply[T <: NewEntity](entity:T, connection:Connection) = new Impl(entity,entity.name,connection)
  def apply[T <: NewEntity](entity:T, name:String,connection:Connection) = new Impl(entity,name,connection)

  class Impl[T <: NewEntity](override val entity:T,
                          override val name:String,
                          override val connection:Connection) extends NewEntityInstance[T]




}