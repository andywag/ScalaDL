package com.simplifide.generate.blocks.proc

import com.simplifide.generate.html.Description


/**
 * Trait which defines an address containing the location of the address as well as the individual items which
 * are contained in this address
 */
trait Address {
    /** Location of address in the map */
    val address:Int
    /** List of Registers included in this address */
    val registers:List[Address.Item]
    /** Append a register to the current address */
    def + (register:Address.Item):Address = Address(this.address,registers ::: List(register))
}

/** Factory methods for creating an address */
object Address {
  /** Creation of an address using the address location as well as the items contained at this location */
  def apply(address:Int, registers:Address.Item*)      = new Impl(address,registers.toList)
  /** Creation of an address as well as a list of items located at this address */
  def apply(address:Int, registers:List[Address.Item]) = new Impl(address,registers)

  /** Default Implementation of Address */
  class Impl(override val address:Int, override val registers:List[Address.Item]) extends Address
  /** Item which is contained in this register
   *
   *  @constructor
   *  @parameter location : Base location (LSB) of item inside this register
   *  @paraemter register : Definition of the register contained at this location
   **/
  class Item(val location:Int, val register:Register) {
    def -- (description:Description) = {
      register.description = Some(description)
    }
  }

}