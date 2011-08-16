package com.simplifide.generate.hier

import com.simplifide.generate.language.Module
import com.simplifide.generate.project.ModuleProvider
import com.simplifide.generate.signal.SignalTrait
import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, SimpleSegment}

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 8/12/11
 * Time: 5:56 PM
 * To change this template use File | Settings | File Templates.
 */

trait Instance extends SimpleSegment {

  val name:String
  val destination:ModuleProvider

  override def toString = name + "(" + destination + ")"

   override def createCode(writer:CodeWriter):SegmentReturn = {
      def createSignals:SegmentReturn = {
        def createSignal(signal:SignalTrait, index:Int):SegmentReturn =
          (if (index != 0) ",\n    " else "\n    ") +  "." + signal.name + "(" + signal.name +")"
        val allSignals = destination.signals.flatMap(_.allSignalChildren).filter(x => (x.isInput || x.isOutput))
        allSignals.zipWithIndex.map(x => createSignal(x._1,x._2)).foldLeft(SegmentReturn.segment(""))(_ + _)
      }
      val out = SegmentReturn.segment(name) + " " + this.destination.name + " (" + createSignals + ");\n\n"
      out
   }
}

object Instance {
   def apply(name:String, destination:ModuleProvider) = new Impl(name,destination)
  /** @deprecated */
   def apply(name:String,source:ModuleProvider,destination:ModuleProvider) = new Impl(name,destination)

   class Impl(override val name:String,
              override val destination:ModuleProvider) extends Instance {

   }
}