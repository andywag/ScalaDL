package com.simplifide.generate.signal

import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, SimpleSegment}
import com.simplifide.generate.blocks.basic.fixed.FixedSelect
import com.simplifide.generate.parser.model.{Clock, Signal}
import com.simplifide.generate.html.Description
import com.simplifide.generate.language.DescriptionHolder
import com.simplifide.generate.parser.SegmentHolder
import com.simplifide.generate.proc.Controls
import com.simplifide.generate.blocks.basic.operator.{Operators, Select}
import collection.mutable.ListBuffer
import com.simplifide.generate.proc.parser.ProcessorSegment
import com.simplifide.generate.blocks.basic.memory.Memory


/**
 * Trait describing a signal
 */

trait SignalTrait extends SimpleSegment with Signal with DescriptionHolder with Controls {

  override val name:String
  /** Type of Signal */
  val opType:OpType
  /** Fixed type of signal */
  val fixed:FixedType
    /** Creates a New Signal */
  def newSignal(nam:String,optype:OpType = this.opType, fix:FixedType = this.fixed):SignalTrait

  /** Method which defines if the signal is an input  */
  override def isInput  = opType.isInput
  /** Method which defines if the signal is an output */
  override def isOutput = opType.isOutput
  /** Returns the type of signal */
  override def getOpType:OpType = opType
  /** Overriding control */
  override val signal:SignalTrait = this

  override val outputs = this.allSignalChildren

  def connect(signal:SignalTrait):Map[SignalTrait,SignalTrait] = Map(this -> signal)

  /** Compares this signal to the input signal. True if same type and name*/
  def generalEquals(signal:SimpleSegment):Boolean = {
    if (signal.isInstanceOf[SignalTrait]) (this.baseSignal.name == signal.asInstanceOf[SignalTrait].baseSignal.name)
    else false
  }

  def baseSignal = this

  /** Method which an indexing of a variable from a clk. Called from the parser x[n-k] */
  def apply(clk:Clock):SimpleSegment = if (clk.delay == 0) this else child(clk.delay)
  /** Returns this variable indexed by the input signal */
  def apply(signal:SignalTrait) = Operators.Slice(this,signal)
  /** Method for indexing a variable. Called from teh parser x[n] */
  def apply(index:Int):SignalTrait =
    if (this.numberOfChildren > 0) child(index) else new SignalSelect(this,index,index)
  /** Creates a slice of a signal */
  def apply(index:(Int,Int)) = new SignalSelect(this,index._1,index._2)//new Select(this,Some(index._1),Some(index._2))

  override def sliceFixed(fixed:FixedType):SimpleSegment = new FixedSelect(this,fixed)

  override def copy(index:Int):SignalTrait = SignalTrait(name + "_" + index, opType, fixed)
  /** Convenience method for copying this signal with a different optype */
  def copyWithOpType(index:Int,optype:OpType):SignalTrait = SignalTrait(name + "_" + index, optype, fixed)


  /** Changes the type for a testbench addition */
  def changeTestType:SignalTrait = SignalTrait(this.name,this.opType.testType,this.fixed)
  /** Changes the type of the signal. Mainly used for Input Output Changes during connections */
  def changeType(typ:OpType):SignalTrait = SignalTrait(this.name,typ,this.fixed)
  /** Reverses the connection for this block */
  def reverseType:SignalTrait = SignalTrait(this.name,this.opType.reverseType,this.fixed)

  val arrayLength = 0

  /** Number of child signals */
  override def numberOfChildren:Int = 0

  override def child(index:Int):SignalTrait = this//slice(index)


  def allSignalChildren:List[SignalTrait] = this.allChildren.map(_.asInstanceOf[SignalTrait])
  /** Create Slice is used for creating the variables in an array whereas slice is
    * used to get the variables. There may be a subtle difference between the 2 methods. Creation of the slice is called
    * when creating the children slice is called on the actual exp
    */
  def createSlice(index:Int, prefix:String = ""):SignalTrait = {
    val cop = this.copy(this.name + "_" + prefix + index)
    cop.description = this.description
    cop
  }
  /** Creates the subsignal associated with this vector index */
  def slice(index:Int):SimpleSegment  = this
  /** Returns all of the children associated with this vector. This method only works on the vector portion
    * of the operation */
  override def children:List[SignalTrait] = List()
  /** Create a list of appendSignal declarations for this appendSignal. This will expand the vector into a larger set of signals */
  def copy(nam:String,optype:OpType=opType,fix:FixedType=fixed):SignalTrait = {
    val cop = newSignal(nam,optype,fix)
    cop.description = this.description
    cop
  }

  def createCode(implicit writer:CodeWriter):SegmentReturn = SegmentReturn(name)

  def sign:SimpleSegment = Select.sign(this)


  /** TODO : Copy of Control Match ... */
  override def createControl(actual:SimpleSegment,statements:ProcessorSegment,index:Int):List[Controls.Value] = {

    this.assignment match {
      case None    => return actual.createControl(null,null,index)
      case Some(x) => x.createControl(actual,statements,index)
    }
  }


  override def controlMatch(actual:SimpleSegment,statements:ProcessorSegment):Boolean = {
    if (actual.isInstanceOf[SignalTrait]) return this.name == actual.name

    val state = statements.getStatement(this)
    state match {
      case None    => false
      case Some(x) => x.input.controlMatch(actual,statements)
    }

  }

}
/**
 * Factory methods for creating new signals
 */
object SignalTrait {

  def apply(name:String) = new Signal(name,OpType.Signal,FixedType.Simple)
  def apply(name:String,optype:OpType) = new Signal(name,optype,FixedType.Simple)
  def apply(name:String,fixed:FixedType) = new Signal(name,OpType.Signal,fixed)
  def apply(name:String,optype:OpType,fixed:FixedType) = new Signal(name,optype,fixed)

  /**
   * Default implementation of SignalTrait
   */
  class Signal(override val name:String,override val opType:OpType,override val fixed:FixedType) extends SignalTrait {

    override val isInput  = opType.isInput
    override val isOutput = opType.isOutput

    override def newSignal(nam:String,optype:OpType,fix:FixedType):SignalTrait = new Signal(nam,optype,fix)
    override def slice(index:Int):SimpleSegment = {
      if (this.numberOfChildren == 0) {
        Select(this,index,index)
      }             // Kind of a kludge shouldn't be required
      else new Signal(name + "_" + index,opType,fixed)
    }
  }

  class InternalArray(name:String,
                      opType:OpType,
                      fixed:FixedType,
                      override val arrayLength:Int = 0) extends Signal(name,opType,fixed) {

  }

  /** Convenience method for creating a unique set of signals */
  def uniqueSignals(signals:List[SignalTrait]):List[SignalTrait] = {
      val sortedSignals = signals.sortBy(_.name)
      val builder = new ListBuffer[SignalTrait]()
      for (signal <- sortedSignals) {
        if (builder.length == 0) builder.append(signal)
        else if (!signal.name.equalsIgnoreCase(builder(builder.length-1).name)) {
          builder.append(signal)
        }
      }
      builder.toList
    }

}
