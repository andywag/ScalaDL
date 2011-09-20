package com.simplifide.generate.project

import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, SimpleSegment}
import com.simplifide.generate.util.{StringOps, FileOps}
import collection.immutable.List._
import java.lang.StringBuffer
import com.simplifide.generate.signal.{RegisterTrait, SignalTrait, SignalDeclaration, OpType}
import java.io.Writer
import com.simplifide.generate.parser.graph.Node
import com.simplifide.generate.hier.{HierarchyInstance, Instance, HierarchyModule}
import com.simplifide.generate.language.{Module, DescriptionHolder, ExtraFile}
import com.simplifide.generate.blocks.basic.flop.ClockControl._
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.hier2.EntityInstance

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 5/31/11
 * Time: 7:19 PM
 * To change this template use File | Settings | File Templates.
 */

/** Trait describing a Impl */
trait ModuleProvider[T <: Module] extends SimpleSegment with HierarchyModule with DescriptionHolder {
  /** Impl Name */
  val name:String
  /** Signals Contained in this module */
  val signals:List[SignalTrait]
  /** Segments Associated with this module if it is a leaf*/
  val segments:List[SimpleSegment]
  /** Instances included in this module */
  val instances:List[Instance[_]]
  /** New Instance Values associated with an Entity */
  val entityInstances:List[EntityInstance]

  /** List of Extra Files associated with this module */
  val extraFiles:List[ExtraFile]
  /** Impl which this is based on */
  val module:T



  override def toString = name

  def allModules:List[ModuleProvider[_]] = {
    instances.flatMap(x => x.destination.allModules) ::: List(this)
  }

  val ioSignals:List[SignalTrait] = signals.flatMap(_.allSignalChildren)//.filter(x => (x.isInput || x.isOutput))

  /*
  def createModule(instances:Option[List[HierarchyInstance]]):HierarchyModule = {
     instances match {
        case None    => this
        case Some(x) => ModuleProvider(this.name,this.module,this.signals,this.segments,this.instances ::: x.map(_.asInstanceOf[Instance]))
      }
  }
  */


  private def createSignalDeclaration(signals:List[SignalTrait], writer:CodeWriter):String = {
    val decs = signals.flatMap(x => SignalDeclaration.createSignalDeclarations(x))
    val builder = new StringBuilder
    decs.foreach(x => builder.append(writer.createCode(x).code))
    return builder.toString
  }

  def createHead2(writer:CodeWriter):String = {
    def singleDec(index:Int,segment:String):String = {
      if (index != 0) return ",\n" + StringOps.writeSpaces(segment,name.length() + 7)
      else            return segment
    }
    //val signals = inputs ::: outputs

    val builder = new StringBuilder
    builder.append("(")
    val tot:List[SignalTrait] = signals.flatMap(_.allSignalChildren)
    val fil = tot.filter(x => !x.opType.isSignal)
    val dec:List[SignalDeclaration] = fil.flatMap(SignalDeclaration.createSignalDeclarationsHead(_))
    dec.zipWithIndex.foreach(x => builder.append(singleDec(x._2,writer.createCode(x._1).code)))
    builder.append(");\n\n")
    builder.toString
  }


  def writeModule(writer:CodeWriter, location:String):SegmentReturn = writeVerilogModule(location)

  def writeVerilogModule(location:String):SegmentReturn     = {
    val writer = CodeWriter.Verilog
    val ret = createCode(writer)
    FileOps.createFile(location, this.name + ".v",ret.code)
    ret

  }

  def createAutoFlops(writer:CodeWriter):String = {
    val builder = new StringBuilder()
    val registers = this.signals.filter(x => x.isInstanceOf[RegisterTrait[_]]).map(x => x.asInstanceOf[RegisterTrait[_]])
    registers.foreach(x => builder.append(writer.createCode(x.createFlop).code))

    builder.toString()
  }


  def createSegment(writer:CodeWriter,segment:SegmentReturn):String = {
    val builder = new StringBuilder
    val extras = segment.extra.map(x => writer.createCode(x))
    extras.foreach(x => builder.append(x.code))
    builder.append(segment.code)
    return builder.toString
  }

  def createCode(writer:CodeWriter):SegmentReturn     = {
    val builder = new StringBuilder()
    //builder.append("module ")
    //builder.append(name)
    //builder.append(this.createHead2(writer))
    builder.append("\n\n// Signal Declarations\n\n")
    val returns:List[SegmentReturn] = segments.map(x => writer.createCode(x))
    val internals = returns.flatMap(x => x.internal)
    builder.append(this.createSignalDeclaration(signals.flatMap(_.allSignalChildren).filter(x => x.opType.isSignal) ::: internals,writer))
    builder.append("\n\n//Instances\n\n")
    //this.instances.foreach(x => writer.createCode(x).code)
    //this.instances.foreach(x => builder.append(writer.createCode(x).code))
    this.entityInstances.foreach(x => builder.append(writer.createCode(x).code))
    builder.append("\n\n// Body\n\n")
    returns.foreach(x => builder.append(createSegment(writer,x)))
    //builder.append("endmodule")
    //builder.append("\n\n")
    return SegmentReturn.segment(builder.toString)
  }

}

object ModuleProvider {

  def apply[T <: Module](name:String,
            module:T,
            signals :List[SignalTrait],
            segments:List[SimpleSegment],
            entityInstances:List[EntityInstance],
            instances:List[Instance[_]] = List(),
            extra:List[ExtraFile] = List()) =
    new Impl(name,module,signals,segments,entityInstances,instances,extra)

  class Impl[T <: Module](override val name:String,
               override val module:T,
               override val signals:List[SignalTrait],
               override val segments:List[SimpleSegment],
               override val entityInstances:List[EntityInstance],
               override val instances:List[Instance[_]],
               override val extraFiles:List[ExtraFile]) extends ModuleProvider[T]

}