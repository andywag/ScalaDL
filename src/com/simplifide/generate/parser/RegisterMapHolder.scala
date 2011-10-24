package com.simplifide.generate.parser

import collection.mutable.{HashMap, ListBuffer}
import com.simplifide.generate.blocks.proc._
import com.simplifide.generate.signal.SignalTrait


/** Trait which provides convenience methods for creating a processor interface */

trait RegisterMapHolder {

  /** Map of Registers contained in the processor interface */
  val registers    = new HashMap[Int,Address]()
  /** Bus which is required for this processor interface */
  val processorBus:ProcessorBus

  /** Method to add a register to the processor interface */
  def sRegister(address:Int, bot:Int, name:String, width:Int):Address.Item = {
    uRegister(address,bot,name,width,Register.READ_WRITE,0)
  }
  /** Method to add a register to the processor interface */
  def uRegister(address:Int, bot:Int, name:String, width:Int,typ:Int = Register.READ_WRITE, default:Long = 0):Address.Item = {
    val reg = Register(name,width,typ)  // Create a new Register
    val item = new Address.Item(bot,reg)
    val addressValue = registers.get(address) match {
      case None    => Address(address,item)
      case Some(x) => x + item
    }
    registers.put(address,addressValue)
    item
  }

  /** Method which creates a register map from them map of registers */
  def createRegisterMap = RegisterMap(registers.toMap)

  /** Creates a read mux for this register map */
  def createReadMux = new AddressDecoder.Read(createRegisterMap,processorBus)
  /** Creates a write mux for this register map */
  def createWriteMux = new AddressDecoder.Write(createRegisterMap,processorBus)
  /** Creates the register signals associated with this interface */
  def createRegisterSignals  = createRegisterMap.outputSignals


}