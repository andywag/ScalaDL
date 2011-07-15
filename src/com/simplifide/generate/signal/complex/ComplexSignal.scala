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
class ComplexSignal(val prototype:SignalTrait) extends BusTrait  {

  override def newBus(name:String,signals:List[SignalTrait]):BusTrait =
    new BusTrait.Bus(name,signals)

  override def newSignal(nam:String,optype:OpType,fix:FixedType):SignalTrait =
    ComplexSignal(nam,fix,optype)

  override def createSlice(index:Int):SignalTrait =
    new ComplexSignal(prototype.createSlice(index))

  override val name   = prototype.name
  override val fixed  = prototype.fixed
  //override val opType:OpType = prototype.opType

  val real =  prototype.copy(this.name + "_re")
  val imag =  prototype.copy(this.name + "_im")

  override val signals:List[SignalTrait] = List(real,imag)

  def debugString:String =  "Complex (" + name + ") " + prototype.fixed.getDescription

   /** Return the sliced value of this signal. Either the vector return or the real or the imaginary 
       portion

   override def getSlice(index:Int):SignalNew = {
     if (proto.vector.arr.size > 0) {
       val nvec = proto.vector.arr.slice(1, proto.vector.arr.size)
       val nproto = proto.copyAsSignalNew(proto.name + "_" + index, None, None,Some(new VectorType(nvec,proto.vector.reg)))
       return new ComplexSignal(nproto)
    }
     if (index == 0) return getReal
     else return getImag
  }
      def getReal:SignalNew =
    return proto.copyAsSignalNew(this.name + "Re" , None, None,Some(new VectorType(List(),vector.reg)))

  def getImag:SignalNew =
    return proto.copyAsSignalNew(this.name + "Im" , None, None,Some(new VectorType(List(),vector.reg)))

  
   def getComplexSlice(index:Int):ComplexSignal = {
	   return getSlice(index).asInstanceOf[ComplexSignal]
   }

        override def copy(nam:String,optype:Option[OpType],fix:Option[FixedType],vec:Option[VectorType]):BaseSignal = {
     val prot = proto.copyAsSignalNew(nam, optype, fix, vec)
     return new ComplexSignal(prot)
   }

        override def getSignalDeclaration:List[SignalDeclarationNew] = {
     if (proto.isInstanceOf[Constant]) return List()
     val base    = this.getFullSignalList
     val signals = base ::: this.getDelaySignalList;
     val decs = signals.flatMap(x => x.opType.getSignalDeclaration(x))
     return decs

   }

   */

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

  def newArray(name:String,fixed:FixedType,optype:OpType=OpType.Signal,len:Int):ArrayTrait2[ComplexSignal]= {
    ArrayTrait2[ComplexSignal](ComplexSignal(name,fixed,optype),len)
  }


  
}


