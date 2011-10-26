package com.simplifide.generate.hier2

import com.simplifide.generate.project2.ModuleProvider
import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, SimpleSegment}
import com.simplifide.generate.signal.SignalTrait

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 9/13/11
 * Time: 2:23 PM
 * To change this template use File | Settings | File Templates.
 */

trait EntityInstance extends SimpleSegment{

  val child:Entity

  override def toString = name + "(" + child + ")"

  // TODO Need to add signal name conversion
  override def createCode(writer:CodeWriter):SegmentReturn = {
      def createSignals:SegmentReturn = {
        def createSignal(signal:SignalTrait, index:Int):SegmentReturn =
          (if (index != 0) ",\n    " else "\n    ") +  "." + signal.name + "(" + this.child.converter.connect(signal).name +")"
        val allSignals = child.signals.flatMap(_.allSignalChildren).filter(x => (x.isInput || x.isOutput))
        allSignals.zipWithIndex.map(x => createSignal(x._1,x._2)).foldLeft(SegmentReturn(""))(_ + _)
      }
      val out = SegmentReturn(child.name) + " " + this.child.connectionName + " (" + createSignals + ");\n\n"
      out
  }

}

object EntityInstance {
  def apply(entity:Entity) = new Impl(entity)

  class Impl(override val child:Entity) extends EntityInstance




}