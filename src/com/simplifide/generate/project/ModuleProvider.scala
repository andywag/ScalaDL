package com.simplifide.generate.project

import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, SimpleSegment}
import collection.immutable.List._
import com.simplifide.generate.signal.{RegisterTrait, SignalTrait, SignalDeclaration, OpType}
import com.simplifide.generate.language.{ DescriptionHolder, ExtraFile}

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 5/31/11
 * Time: 7:19 PM
 * To change this template use File | Settings | File Templates.
 */

/** Trait describing a Impl */
trait ModuleProvider extends SimpleSegment  with DescriptionHolder {
  /** Impl Name */
  val name:String
  /** Signals Contained in this module */
  val signals:List[SignalTrait]
  /** Segments Associated with this module if it is a leaf*/
  val segments:List[SimpleSegment]
  /** New Instance Values associated with an Entity */
  val entityInstances:List[EntityInstance[_]]

  /** List of Extra Files associated with this module */
  val extraFiles:List[ExtraFile]
  /** Impl which this is based on */
  val module:Module

  override def toString = name


  /** Create the signal declarations for this module */
  private def createSignalDeclaration(signals:List[SignalTrait], writer:CodeWriter):String = {
    val decs = signals.flatMap(x => SignalDeclaration(x))
    val builder = new StringBuilder
    decs.foreach(x => builder.append(writer.createCode(x).code))
    return builder.toString
  }

  /** Creates the flops for registers defined in the module */
  private def createAutoFlops(writer:CodeWriter):String = {
    val builder = new StringBuilder()
    val registers = this.signals.filter(x => x.isInstanceOf[RegisterTrait[_]]).map(x => x.asInstanceOf[RegisterTrait[_]])
    registers.foreach(x => builder.append(writer.createCode(x.createFlop).code))

    builder.toString()
  }

  /** Create the segments for this module */
  private def createSegment(writer:CodeWriter,segment:SegmentReturn):String = {
    val builder = new StringBuilder
    val extras = segment.extra.map(x => writer.createCode(x))
    extras.foreach(x => builder.append(x.code))
    builder.append(segment.code)
    return builder.toString
  }

  def createCode(implicit writer:CodeWriter):SegmentReturn     = {
    val builder = new StringBuilder()

    builder.append("\n\n// Signal Declarations\n\n")
    val returns:List[SegmentReturn] = segments.map(x => writer.createCode(x)).filter(_ != null)
    val internals = returns.flatMap(x => x.internal).filter(x => !x.isInput && !x.isOutput)
    val allSignals = SignalTrait.uniqueSignals(signals.flatMap(_.allSignalChildren).filter(x => x.opType.isSignal) ::: internals)
    builder.append(this.createSignalDeclaration(allSignals,writer))
    builder.append(this.createAutoFlops(writer))
    builder.append("\n\n//Instances\n\n")

    this.entityInstances.foreach(x => builder.append(writer.createCode(x).code))
    builder.append("\n\n// Body\n\n")
    returns.foreach(x => builder.append(createSegment(writer,x)))

    return SegmentReturn(builder.toString)
  }

}

/**
 * Factory methods for Module Provider
 *
 */
object ModuleProvider {

  def apply(name:String,
            module:Module,
            signals :List[SignalTrait],
            segments:List[SimpleSegment],
            entityInstances:List[EntityInstance[_]],
            extra:List[ExtraFile] = List()) =
    new Impl(name,module,signals,segments,entityInstances,extra)

  class Impl(override val name:String,
               override val module:Module,
               override val signals:List[SignalTrait],
               override val segments:List[SimpleSegment],
               override val entityInstances:List[EntityInstance[_]],
               override val extraFiles:List[ExtraFile]) extends ModuleProvider

}