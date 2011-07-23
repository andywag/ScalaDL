package com.simplifide.generate.signal.complex

import com.simplifide.generate.signal._
import com.simplifide.generate.signal.SignalTrait._

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */


/** Complex Number which contains a real and imaginary section
    For the most part this is treated like a vector where
    the first part is the real and the second part is the imaginary
 */

class ComplexSignal(val prototype:SignalTrait) extends SignalTrait  {

  override val name:String       = prototype.name
  override val opType:OpType     = prototype.opType
  override val fixed:FixedType   = prototype.fixed

  val real =  prototype.copy(this.name + "_re")
  val imag =  prototype.copy(this.name + "_im")

  override def newSignal(nam:String,optype:OpType,fix:FixedType):SignalTrait =
    ComplexSignal(nam,fix,optype)

  override def createSlice(index:Int):SignalTrait =
    new ComplexSignal(prototype.createSlice(index))




  //override val signals:List[SignalTrait] = List(real,imag)

  def debugString:String =  "Complex (" + name + ") " + prototype.fixed.getDescription


  override def copy(nam:String,optype:OpType=opType,fix:FixedType=fixed):SignalTrait =
    new ComplexSignal(prototype.copy(name,optype,fixed))

  
   /** Copies the signal with different options */

   


  
}

object ComplexSignal {

  /** @deprecated New Signal based on a name and a fixed point type */
  def newComplex(name:String,fixed:FixedType ) =
    new ComplexSignal(SignalTrait(name,fixed))
  /** @deprecated New Signal based on a name a fixed point type and a length */
  def newComplex(name:String,optype:OpType,fixed:FixedType) =
    new ComplexSignal(SignalTrait(name,optype,fixed))
  /** @deprecated New Signal based on a name a fixed point type and a length */
  //def newComplex(name:String,optype:OpType,fixed:FixedType,len:Int) =
  //  new ComplexSignal(SignalTrait(name,optype,fixed,len))
  /** @deprecated New Signal based on a name a fixed point type and a length */
  /*def newComplex(name:String,opType:OpType,fixed:FixedType,vector:VectorType):ComplexSignal = {
    val sig = SignalTrait(name,opType,fixed,vector)
    return new ComplexSignal(sig)
  }*/

  def apply(name:String,fixed:FixedType,optype:OpType=OpType.Signal) =
    new ComplexSignal(SignalTrait(name,optype,fixed))

  def newArray(name:String,fixed:FixedType,optype:OpType=OpType.Signal,len:Int):ArrayTrait[ComplexSignal]= {
    ArrayTrait[ComplexSignal](ComplexSignal(name,fixed,optype),len)
  }


  
}


