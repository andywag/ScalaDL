package com.simplifide.generate.parser

import model.{Signal, Model, SignalType}
import com.simplifide.generate.signal.Constant._
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.signal.RegisterTrait._
import com.simplifide.generate.signal._
import com.simplifide.generate.signal.Bus._
import com.simplifide.generate.signal.ArrayTrait._
import complex.ComplexSignal._
import complex.{ComplexConstant, ComplexSignal}


/** Convenience Methods for Creating Signals */
trait SignalMethods {

  val INPUT  = OpType.Input
  val OUTPUT = OpType.Output
  val WIRE   = OpType.Signal
  val REG    = OpType.Register
  val REGOUT = OpType.ModuleRegOutput

  def appendSignal[T <: SignalTrait](signal:T):T = {
    signal
  }

  def appendSignals(signals:List[SignalTrait]) = {
    signals.foreach(x => this.appendSignal(x))
  }


     // Set of Signal Declarations
  def signal(name:String, typ:SignalType = SignalType.SignalTypeImpl,fixed:Model.Fixed = Model.Fixed(1,0)):SignalTrait = {
    appendSignal(ObjectFactory.Signal(name,typ,fixed)(List()))
  }
  /** Convenience method for creating a appendSignal */
  def array(name:String, typ:SignalType = SignalType.SignalTypeImpl,fixed:Model.Fixed = Model.Fixed(1,0))(arr:Int*):ArrayTrait[SignalTrait] = {
    appendSignal(ObjectFactory.Signal(name,typ,fixed)(arr.toList)).asInstanceOf[ArrayTrait[SignalTrait]]
  }



  /** Create a register from a appendSignal while specifying the clock */
  def register[T <: SignalTrait](signal1:T)(length:Int)(implicit clk:ClockControl):RegisterTrait[T] = {
    appendSignal(RegisterTrait(signal1,length,clk))
  }

  def register(name:String, typ:SignalType = OpType.Signal,fixed:Model.Fixed = Model.Fixed(1,0))
              (length:Int)(implicit clk:ClockControl):RegisterTrait[SignalTrait] = {
    val sig:SignalTrait = signal(name,typ,fixed)
    register(sig)(length)(clk)
  }

  /** Convenience Methods for creating Signed and Unsigned Fixed Point Types */
  def S(width:Int,fraction:Int)        = signed(width,fraction)
  def U(width:Int,fraction:Int)        = unsigned(width,fraction)
  def signed(width:Int,fraction:Int)   = FixedType.signed(width,fraction)
  def unsigned(width:Int,fraction:Int) = FixedType.unsigned(width,fraction)

  /** Method to generate a constant based on an input floating point number */
   def constant(value:Double) = {
     val values = List.tabulate(32)(i => value*scala.math.pow(2.0,i-16))
     val intValue = values.reverse.indexWhere(x => scala.math.floor(x) == 0)
     val fracValue = values.indexWhere(x => (x - scala.math.floor(x) == 0))
     Constant(value,signed(fracValue - intValue-1,fracValue-16))
  }

  def constant(value:Double,fixed:Model.Fixed = Model.NoFixed) =
    ObjectFactory.Constant("",value,fixed)



    /** Convenience method for creating a appendSignal */
  def bus(name:String, typ:BusType):Bus =  {
    appendSignal(Bus(name,typ))

  }
  /** Convenience method for creating a appendSignal */
  def busArray(name:String, typ:BusType)(arr:Int*):SignalTrait =  {
    val bus = Bus(name,typ)
    val sig = ArrayTrait(bus,arr(0))
    appendSignal(sig)
  }

  // Complex Signals

  /** Standard Complex Signal Creation */
  def complex(name:String, typ:OpType = OpType.Signal,fixed:FixedType = FixedType.None):ComplexSignal =
    appendSignal(ComplexSignal(name,typ,fixed))

  def complex_reg(name:String, typ:OpType = OpType.Signal,fixed:FixedType = FixedType.None)
                 (length:Int)(implicit clk:ClockControl) = {
    val signal1 = appendSignal(ComplexSignal(name,typ,fixed))
    register(signal1)(length)(clk)
  }

  def complex_array(name:String, typ:OpType = OpType.Signal,fixed:FixedType = FixedType.None)
                 (length:Int)(implicit clk:ClockControl):ArrayTrait[ComplexSignal] = {
    val signal1 = ComplexSignal(name,typ,fixed)
    appendSignal(ArrayTrait(signal1,length))
  }

  def complex_array2(name:String, typ:OpType = OpType.Signal,fixed:FixedType = FixedType.None)
                 (length1:Int)(length2:Int)(implicit clk:ClockControl):ArrayTrait[ArrayTrait[ComplexSignal]] = {
    val signal1 = appendSignal(ComplexSignal(name,typ,fixed))
    val arr1 = ArrayTrait(signal1,length1)
    appendSignal(ArrayTrait(arr1,length2))
  }


  /** Complex Constant Constructor */
  def complex(real:Double, imag:Double) = ComplexConstant(real,imag)
  def complex(real:Double, imag:Double, fixed:FixedType) = ComplexConstant(fixed,real,imag)



  /** Signal Conjugation */
  def conj(signal:ComplexSignal) = conjugate(signal)
  def conjugate(signal:ComplexSignal) = ComplexSignal.Conjugate(signal.prototype)


}