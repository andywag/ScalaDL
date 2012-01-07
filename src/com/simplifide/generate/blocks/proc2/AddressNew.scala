package com.simplifide.generate.blocks.proc2

import com.simplifide.generate.blocks.proc.Register
import com.simplifide.generate.html.Description


/**
 * Trait which defines an address containing the location of the address as well as the individual items which
 * are contained in this address
 */
trait AddressNew {
  /** Location of address in the map */
  val address:Int
  /** List of Registers included in this address */
  val registers:List[FullRegister]
  /** Append a register to the current address */
  val description:Description
    //def + (register:List[FullRegister]):AddressNew = AddressNew(this.address,registers ::: List(register))

  def comment(description:Description) = new AddressNew.Impl(address,registers,description)
}

/** Factory methods for creating an address */
object AddressNew {

  /** Creation of an address as well as a list of items located at this address */
  def apply(address:Int, registers:List[FullRegister]) = new Impl(address,registers)

  /** Default Implementation of AddressNew */
  class Impl(override val address:Int,
             override val registers:List[FullRegister],
             override val description:Description = Description.Empty) extends AddressNew


}

