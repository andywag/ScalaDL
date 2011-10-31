package com.simplifide.generate.signal


/**
 * Verilog parameter
 */
trait ParameterTrait extends SignalTrait {
  /** Value of the parameter */
  val value:Int
  /** Fixed type of the parameter */
  override val fixed = FixedType.Simple
  /** Operating type of the parameter */
  override val opType = OpType.Param

  def newSignal(nam:String,optype:OpType = this.opType,fix:FixedType = this.fixed):SignalTrait =
    ParameterTrait(nam,this.value)

}

/**
 * Factory method for creating a verilog parameter
 */
object ParameterTrait {
  def apply(name:String, value:Int) = new Implementation(name,value)

  class Implementation(override val name:String, override val value:Int) extends ParameterTrait {

  }
}