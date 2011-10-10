package com.simplifide.generate.parser

import collection.mutable.{HashMap, ListBuffer}
import com.simplifide.generate.blocks.proc._
import com.simplifide.generate.signal.SignalTrait

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 8/17/11
 * Time: 6:38 PM
 * To change this template use File | Settings | File Templates.
 */

trait RegisterMapHolder {

  //implicit val registerHolder:RegisterMapHolder = this

  val registers    = new HashMap[Int,Address]()
  val processorBus:ProcessorBus

  def sRegister(address:Int, bot:Int, name:String, width:Int):Address.Item = {
    uRegister(address,bot,name,width,Register.READ_WRITE,0)
  }



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

  def createRegisterMap = RegisterMap(registers.toMap)

  def createReadMux = new AddressDecoder.Read(createRegisterMap,processorBus)
  def createWriteMux = new AddressDecoder.Write(createRegisterMap,processorBus)
  def createRegisterSignals  = createRegisterMap.outputSignals


}