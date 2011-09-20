package com.simplifide.generate.signal

import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, SimpleSegment}
import com.simplifide.generate.blocks.basic.fixed.FixedSelect
import com.simplifide.generate.parser.model.{Clock, Signal}
import com.simplifide.generate.blocks.basic.operator.Select
import com.simplifide.generate.html.Description
import com.simplifide.generate.language.DescriptionHolder
import com.simplifide.generate.parser.SegmentHolder
import com.simplifide.generate.proc.Controls

/*
* To change this template, choose Tools | Templates
* and open the template in the editor.
*/


trait SignalTrait extends SimpleSegment with Signal with DescriptionHolder {

  override val name:String
  val opType:OpType
  val fixed:FixedType

  def baseSignal = this

  def generalEquals(signal:SimpleSegment):Boolean = {
    if (signal.isInstanceOf[SignalTrait])
      (this.baseSignal.name == signal.asInstanceOf[SignalTrait].baseSignal.name)
    else
      false
  }


  override def apply(clk:Clock) = child(clk.delay).asInstanceOf[Signal]
  override def apply(index:Int) = child(index).asInstanceOf[Signal]

  override def sliceFixed(fixed:FixedType):SimpleSegment = new FixedSelect(this,fixed)
  override def copy(index:Int):SignalTrait = SignalTrait(name + "_" + index, opType, fixed)

  /** Changes the type of the signal. Mainly used for Input Output Changes during connections */
  def changeType(typ:OpType):SignalTrait = SignalTrait(this.name,typ,this.fixed)
  /** Reverses the connection for this block */
  def reverseType:SignalTrait = SignalTrait(this.name,this.opType.reverseType,this.fixed)

  val arrayLength = 0

  /** Number of child signals */
  override def numberOfChildren:Int = 0

  override def child(index:Int):SimpleSegment = slice(index)
  /** Creates a New Signal */
  def newSignal(nam:String,
                optype:OpType = this.opType,
                fix:FixedType = this.fixed):SignalTrait

  def allSignalChildren:List[SignalTrait] = this.allChildren.map(_.asInstanceOf[SignalTrait])
  /** Create Slice is used for creating the variables in an array whereas slice is
    * used to get the variables. There may be a subtle difference between the 2 methods. Creation of the slice is called
    * when creating the children slice is called on the actual exp
    */
  def createSlice(index:Int):SignalTrait = {
    val cop = this.copy(this.name + "_" + index)
    cop.description = this.description
    cop
  }
  /** Creates the subsignal associated with this vector index */
  def slice(index:Int):SignalTrait  = this
  /** Returns all of the children associated with this vector. This method only works on the vector portion
    * of the operation */
  override def children:List[SignalTrait] = List()
  /** Create a list of appendSignal declarations for this appendSignal. This will expand the vector into a larger set of signals */
  def copy(nam:String,optype:OpType=opType,fix:FixedType=fixed):SignalTrait = {
    val cop = newSignal(nam,optype,fix)
    cop.description = this.description
    cop
  }

  def createCode(writer:CodeWriter):SegmentReturn = SegmentReturn.segment(name)

  def sign:SimpleSegment = Select.sign(this)


  /** TODO : Copy of Control Match ... */
  override def createControl(actual:SimpleSegment,statements:SegmentHolder,index:Int):List[Controls] = {
    val state = statements.getStatement(this)
    state match {
      case None    => return List()
      case Some(x) => x.input.createControl(actual,statements,index)
    }
    //if (actual.isInstanceOf[SignalTrait]) return List()



  }


  override def controlMatch(actual:SimpleSegment,statements:SegmentHolder):Boolean = {
    if (actual.isInstanceOf[SignalTrait]) return this.name == actual.name

    val state = statements.getStatement(this)
    state match {
      case None    => false
      case Some(x) => x.input.controlMatch(actual,statements)
    }

  }




}

object SignalTrait {

  def apply(name:String) = new Signal(name,OpType.Signal,FixedType.Simple)
  def apply(name:String,optype:OpType) = new Signal(name,optype,FixedType.Simple)
  def apply(name:String,fixed:FixedType) = new Signal(name,OpType.Signal,fixed)
  def apply(name:String,optype:OpType,fixed:FixedType) = new Signal(name,optype,fixed)

  def newSignal(name:String) = new Signal(name,OpType.Signal,FixedType.Simple)
  /** Creates a new single bit appendSignal with the OpType optype */
  def newSignal(name:String,optype:OpType) = new Signal(name,optype,FixedType.Simple)
  /** Creates a new appendSignal with a fixed type as well and programmable optype */
  def newSignal(name:String,fixed:FixedType) = new Signal(name,OpType.Signal,fixed)
  /** Creates a new appendSignal with a fixed type as well and programmable optype */
  def newSignal(name:String,optype:OpType,fixed:FixedType) = new Signal(name,optype,fixed)
  /** Creates a new appendSignal */

  class Signal(override val name:String,override val opType:OpType,override val fixed:FixedType) extends SignalTrait {

    override val isInput  = opType.isInput
    override val isOutput = opType.isOutput

    override def newSignal(nam:String,optype:OpType,fix:FixedType):SignalTrait = new Signal(nam,optype,fix)
    override def slice(index:Int):SignalTrait = {
      if (this.numberOfChildren == 0) this             // Kind of a kludge shouldn't be required
      else new Signal(name + "_" + index,opType,fixed)
    }
  }
}
