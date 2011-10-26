package com.simplifide.generate.hier2

import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.project2.ModuleProvider
import com.simplifide.generate.generator.{SegmentReturn, CodeWriter}
import com.simplifide.generate.signal.{SignalDeclaration, OpType, SignalTrait}
import com.simplifide.generate.util.{FileOps, StringOps}
import com.simplifide.generate.parser.SignalMethods
import collection.mutable.ListBuffer

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 9/7/11
 * Time: 1:37 PM
 * To change this template use File | Settings | File Templates.
 */

/**
 * This class contains information about the modules hierarchy as well as global information used in creating
 * the modules. This class is also instrumental in creating the hierarchy for the design
 */
class Entity(val name:String,
             val connectionName:String,
             val converter:Connection)(implicit val clk:ClockControl) extends SignalMethods {

  /** Input/Output Signals for this Entity */
  val signals:List[SignalTrait] = List()
  /** Children of this Entity */
  val entities:List[Entity] = List()

  def inputSignals:List[SignalTrait]  = signals.filter(_.isInput)
  def outputSignals:List[SignalTrait] = signals.filter(_.isOutput)
  def ioSignals:List[SignalTrait]     = inputSignals ::: outputSignals

  /** Create Recursive List of Children */
  def children:List[Entity] = entities ::: entities.flatMap(x => x.children)
  /** First pass for creating the connections for the entities. Attaches the module inputs */
  def inputPass:Entity = this
  def outputPass(outputs:Option[List[SignalTrait]]):Entity = this

  /** Write this module to a file */
  def writeModule(location:String):SegmentReturn     = {
    val writer = CodeWriter.Verilog
    val ret = createCode(writer)
    FileOps.createFile(location, this.name + ".v",ret.code)
    ret
  }

  def createModule:ModuleProvider[_] = {
    ModuleProvider(name,null,this.signals,this.entities.map(x => EntityInstance(x)),List(),List())
  }


  def createHead2(writer:CodeWriter):String = {
    def singleDec(index:Int,segment:String):String = {
      if (index != 0) return ",\n" + StringOps.writeSpaces(segment,name.length() + 7)
      else            return segment
    }
    val builder = new StringBuilder
    builder.append("(")
    val tot:List[SignalTrait] = signals.flatMap(_.allSignalChildren)
    val fil = tot.filter(x => !x.opType.isSignal)
    val dec:List[SignalDeclaration] = fil.flatMap(SignalDeclaration.createSignalDeclarationsHead(_))
    dec.zipWithIndex.foreach(x => builder.append(singleDec(x._2,writer.createCode(x._1).code)))
    builder.append(");\n\n")
    builder.toString
  }



  def createCode(writer:CodeWriter):SegmentReturn = {
    val builder = new StringBuilder()
    builder.append("module ")
    builder.append(name)
    builder.append(this.createHead2(writer))
    builder.append(this.createModule.createCode(CodeWriter.Verilog))
    builder.append("endmodule")
    builder.append("\n\n")
    return SegmentReturn(builder.toString)
  }

}

object Entity {

  class Root(name:String,
             connectionName:String,
             converter:Connection = Connection.Default)(override implicit val clk:ClockControl) extends Branch(name,connectionName,converter) {

    val outputs:Option[List[SignalTrait]] = None
    def connect:Entity = {
      val inputPassRoot = this.inputPass
      inputPassRoot.outputPass(outputs)
    }
  }

  abstract class Leaf(name:String,
                      connectionName:String,
                      converter:Connection = Connection.Default)(override implicit val clk:ClockControl) extends Entity(name,connectionName,converter) {
    //def createModule[T <: Module]:ModuleProvider[T]

  }

  class Branch(name:String,
               connectionName:String,
               converter:Connection = Connection.Default)(override implicit val clk:ClockControl) extends Entity(name,connectionName,converter) {

    private def containsOutput(signal:SignalTrait,outputs:List[SignalTrait]) =
      outputs.filter(_.name.equalsIgnoreCase(signal.name)).size > 0


    def uniqueSignals(signals:List[SignalTrait]):List[SignalTrait] = {
      signals.sortBy(_.name)
      val builder = new ListBuffer[SignalTrait]()
      for (signal <- signals) {
        if (builder.length == 0) builder.append(signal)
        else if (!signal.name.equalsIgnoreCase(builder(builder.length-1).name)) {
          builder.append(signal)
        }
      }
      builder.toList
    }

    /** Create the signals in the branch module as a function of the inputs. This branch adds the inputs up the hierarchy
     *  which don't already exist as outputs from it's siblings. In the latter case a wire is appended
     **/

    override def inputPass:Entity = {
      // Construct the child modules
      val newEntities = entities.map(x => x.inputPass)
      val signals = newEntities.flatMap(_.signals).flatMap(_.allSignalChildren).map(converter.connect(_))
      val inputSignals1 = signals.flatMap(_.allSignalChildren).filter(_.isInput).sortBy(_.name)
      val inputSignals = uniqueSignals(inputSignals1)
      val outputSignals = signals.flatMap(_.allSignalChildren).filter(_.isOutput)
      val newInputs = inputSignals.filter(x => !containsOutput(x,outputSignals))
      val newWires  = inputSignals.filter(x => containsOutput(x,outputSignals)).map(x => x.changeType(OpType.Signal))
      new ExpandedEntity(this,this.signals ::: newInputs ::: newWires,newEntities)
    }
     /** Second pass for creating the connections for the entities. Filters the modules outputs */
  override def outputPass(outputs:Option[List[SignalTrait]]):Entity = {
    def containsSignal(signal:SignalTrait) =
      outputs match {
        case Some(x) => x.filter(_.name.equalsIgnoreCase(signal.name)).size > 0
        case None    => false
      }
    val newEntities = entities.map(x => x.outputPass(outputs))
    val wires = this.signals.flatMap(_.allSignalChildren).filter(x => !x.isOutput && !x.isInput)
    val outputSignals = newEntities.flatMap(_.signals).flatMap(_.allSignalChildren).filter(_.isOutput).filter(x => !containsSignal(x)).filter(x => !containsOutput(x,wires))
    new ExpandedEntity(this,this.signals ::: outputSignals,newEntities)
  }

  }

}