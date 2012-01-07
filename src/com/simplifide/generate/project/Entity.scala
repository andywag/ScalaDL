package com.simplifide.generate.project

import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.util.{FileOps, StringOps}
import collection.mutable.ListBuffer
import com.simplifide.generate.parser.{ModuleParser, SignalMethods}
import com.simplifide.generate.generator.{ComplexSegment, SimpleSegment, SegmentReturn, CodeWriter}
import com.simplifide.generate.signal.{SignalTrait, SignalDeclaration, OpType}
import com.simplifide.generate.proc.{Controls, ControlBase}

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
 *
 * @parameter name Name of Entity
 * @parameter connectionName Name of the Connection for this block
 * @parameter converter Class which converts the signals for the connections
 *
 */
class Entity(val name:String,
             val connectionName:String,
             val converter:Connection)(implicit val clk:ClockControl) extends ModuleParser with ControlBase {

  /** Input/Output Signals for this Entity */
  val entitySignals:List[SignalTrait] = List()
  /** Entity Instances */
  val instances:List[EntityInstance[_]] = List()
  /** Children of this Entity */
  lazy val entities:List[Entity] = instances.map(x => x.entity.asInstanceOf[Entity])
  ///** Segments contained in the module */
  //val segments:List[SimpleSegment] = List()

  /** List of Input Signals in this Entity */
  def inputSignals:List[SignalTrait]  = entitySignals.filter(_.isInput)
  /** List of Output Signals in this Entity */
  def outputSignals:List[SignalTrait] = entitySignals.filter(_.isOutput)
  /** Complete List of Input and Output Signals */
  def ioSignals:List[SignalTrait]     = inputSignals ::: outputSignals
  /** Internal signals contained in this entity */
  val internalSignals:List[SignalTrait] = List()
  /** Internal signals contained in this entity */
  val internalStatements:List[SimpleSegment] = List()
  /** Create Recursive List of Children */
  def children:List[Entity] = (entities ::: entities.flatMap(x => x.children)).toSet.toList
  /** First pass for creating the connections for the entities. Attaches the module inputs */
  def inputPass:Entity = this
  /** Second pass which potentially removes the unused outputs */
  def outputPass(outputs:Option[List[SignalTrait]]):Entity = this

  /** Write the entity and module to a file */
  def writeModule(location:String):SegmentReturn     = {
    val writer = CodeWriter.Verilog
    val ret = createCode(writer)
    FileOps.createFile(location, this.name + ".v",ret.code)
    ret
  }
  /** Creates the body of this module. By default this attaches the instances associated with this entity */
  override def createModule:ModuleProvider = {
    val states = this.statements.flatMap(_.split).toList.map(_.asInstanceOf[SimpleSegment])
    ModuleProvider(name,null,this.entitySignals ::: this.internalSignals,states,this.instances,List())
  }

  def extraSignals = {
    val states = this.statements.flatMap(_.split).toList.map(_.asInstanceOf[SimpleSegment])
    val complex = states.filter(x => x.isInstanceOf[ComplexSegment]).map(x => x.asInstanceOf[ComplexSegment])
    val internals = complex.flatMap(_.signals).filter(x => x.isInput || x.isOutput).toList
    val complexHolder = states.filter(x => x.isInstanceOf[ComplexSegment.Holder]).map(x => x.asInstanceOf[ComplexSegment.Holder])
    val internalsHolder = complexHolder.flatMap(_.signals).filter(x => x.isInput || x.isOutput).toList
    internals ::: internalsHolder ::: this.signals.filter(x => x.isInput || x.isOutput).toList
  }

  // TODO Clean up this code
  /** Create the code for the head of the module */
  def createHead2(writer:CodeWriter):String = {
    def singleDec(index:Int,segment:String):String = {
      if (index != 0) return ",\n" + StringOps.writeSpaces(segment,name.length() + 7)
      else            return segment
    }
    def uniqueList(signals:List[SignalTrait]):List[SignalTrait] = {
      val newSignals = signals.sortBy(_.name)
      val buffer = new ListBuffer[SignalTrait]()
      if (newSignals.length > 0) buffer.append(newSignals(0))
      for (i <- 1 until newSignals.length) {
        if (!newSignals(i-1).name.equalsIgnoreCase(newSignals(i).name)) buffer.append(newSignals(i))
      }
      buffer.toList
    }
    val builder = new StringBuilder
    builder.append("(")
    val tot:List[SignalTrait] = uniqueList( (entitySignals ::: extraSignals).flatMap(_.allSignalChildren))
    val fil = tot.filter(x => !x.opType.isSignal)
    val dec:List[SignalDeclaration] = fil.flatMap(SignalDeclaration.head(_))
    dec.zipWithIndex.foreach(x => builder.append(singleDec(x._2,writer.createCode(x._1).code)))
    builder.append(");\n\n")
    builder.toString
  }



  def createCode(writer:CodeWriter):SegmentReturn = {
    implicit val writ:CodeWriter = writer
    val mod = this.createModule
    return SegmentReturn("module ") + name + this.createHead2(writer) + mod + "endmodule\n\n"
  }

  // List of Controls contained in this entity
  //override val controls
  lazy val createControls:List[Controls] = {
    val state:List[Controls]  = this.statements.map(x => x.asInstanceOf[SimpleSegment]).flatMap(_.controls).toList
    val entity:List[Controls] = this.entities.flatMap(_.createControls).toList
    state ::: entity
  }

  implicit def Instance2Entity[T <: Entity](instance:EntityInstance[T]):T = instance.entity
    //if (instance != null) instance.entity else null


}

object Entity {
  /** Root Entity for a project */
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

    /**
     *  Create the signals in the branch module as a function of the inputs. This branch adds the inputs up the hierarchy
     *  which don't already exist as outputs from it's siblings. In the latter case a wire is appended
     **/

    override def inputPass:Entity = {
      // Construct the child instances
      val newInstances = instances.map(x => x.inputPass)
      // Convert the signals to the name for this module
      val signals =  newInstances.flatMap(_.allSignals)
      // Total signals in this module --- Shouldn't be required
      val allSignals = signals.flatMap(_.allSignalChildren)
      // Find a unique set of input signals
      val inputSignals = SignalTrait.uniqueSignals(allSignals.filter(_.isInput))
      // Find a unique set of outptu signals
      val outputSignals = SignalTrait.uniqueSignals(allSignals.filter(_.isOutput))
      // Creates a New Inputs when the Input is not Attached to An Output --- Should also be other signals
      val newInputs = inputSignals.filter(x => !containsOutput(x,outputSignals ::: this.signals.toList))
      // Creates a new set of wires when there is an input or output
      val newWires  = inputSignals.filter(x => containsOutput(x,outputSignals)).map(x => x.changeType(OpType.Signal))
      // Returns a new entity

      new ExpandedEntity(this,this.internalSignals ::: this.signals.toList,this.entitySignals ::: newInputs ::: newWires,newInstances,
        this.statements.map(x => x.create).toList ::: this.internalStatements)
    }
     /** Second pass for creating the connections for the entities. Filters the modules outputs */
  override def outputPass(outputs:Option[List[SignalTrait]]):Entity = {
    def containsSignal(signal:SignalTrait) =
      outputs match {
        case Some(x) => x.filter(_.name.equalsIgnoreCase(signal.name)).size > 0
        case None    => false
      }
    // Creates the entities from the child modules
    val newInstances = this.instances.map(x => x.outputPass(outputs))
    // Finds the wires from this module
    val wires = this.entitySignals.flatMap(_.allSignalChildren).filter(x => !x.isOutput && !x.isInput)
    // Returns a list of all output signals from the child modules
    val outputSignals1 = newInstances.flatMap(_.allSignals).flatMap(_.allSignalChildren).filter(_.isOutput)
    // Removes the output signals from the total list of signasl
    val outputSignals  = outputSignals1.filter(x => !containsSignal(x)).filter(x => !containsOutput(x,wires))
    new ExpandedEntity(this,this.internalSignals ::: wires ::: this.signals.toList,this.entitySignals ::: outputSignals ,newInstances,
      this.statements.map(x => x.create).toList ::: this.internalStatements)
  }

  }

}