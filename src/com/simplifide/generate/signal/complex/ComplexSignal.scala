package com.simplifide.generate.signal.complex

import com.simplifide.generate.signal._
import com.simplifide.generate.signal.SignalTrait._
import com.simplifide.generate.generator.SimpleSegment

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */


/** Complex Number which contains a real and imaginary section
    For the most part this is treated like a vector where
    the first part is the real and the second part is the imaginary
 */

class ComplexSignal(val prototype:SignalTrait) extends SignalTrait {

  override val name:String       = prototype.name
  override val opType:OpType     = prototype.opType
  override val fixed:FixedType   = prototype.fixed


  val real =  prototype.copy(this.name + "_re")
  val imag =  prototype.copy(this.name + "_im")

  val conjugate = false

  /** Changes the type of the signal. Mainly used for Input Output Changes during connections */
  override def changeType(typ:OpType):SignalTrait = new ComplexSignal(prototype.changeType(typ))
  /** Changes the type of the signal. Mainly used for Input Output Changes during connections */
  override def reverseType:SignalTrait = new ComplexSignal(prototype.reverseType)

  override def newSignal(nam:String,optype:OpType,fix:FixedType):SignalTrait =
    ComplexSignal(nam,optype,fix)

  override def createSlice(index:Int):SignalTrait = //if (index == 0) this.real else if (index ==1) this.imag else this
    new ComplexSignal(prototype.createSlice(index))

  override def numberOfChildren = 2

  override def child(index:Int):SignalTrait = if (index == 0) this.real else if (index ==1) this.imag else this
  override def children:List[SignalTrait] = List(real,imag)
  override def allChildren:List[SignalTrait] = List(real,imag)

  override def slice(index:Int):SignalTrait  = this



  //override val signals:List[SignalTrait] = List(real,imag)

  def debugString:String =  "Complex (" + name + ") " + prototype.fixed.getDescription


  override def copy(nam:String,optype:OpType=opType,fix:FixedType=fixed):SignalTrait =
    new ComplexSignal(prototype.copy(name,optype,fixed))

  
   /** Copies the appendSignal with different options */

   


  
}

object ComplexSignal {


  def apply(name:String,fixed:FixedType = FixedType.None) =
    new ComplexSignal(SignalTrait(name,OpType.Signal,fixed))

  def apply(name:String,optype:OpType,fixed:FixedType) =
    new ComplexSignal(SignalTrait(name,optype,fixed))

  def newArray(name:String,fixed:FixedType,optype:OpType=OpType.Signal,len:Int):ArrayTrait[ComplexSignal]= {
    ArrayTrait[ComplexSignal](ComplexSignal(name,optype,fixed),len)
  }

  class Conjugate(prototype:SignalTrait) extends ComplexSignal(prototype) {
      override val conjugate = true
  }

  object Conjugate {
    def apply(signal:SignalTrait) = new ComplexSignal.Conjugate(signal)
  }


  
}


