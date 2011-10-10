package com.simplifide.generate.blocks.proc

import com.simplifide.generate.html.Description

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 8/16/11
 * Time: 3:31 PM
 * To change this template use File | Settings | File Templates.
 */

trait Address {
    /** Location of address in the map */
    val address:Int
    /** List of Registers included in this address */
    val registers:List[Address.Item]
    /** Append a register to the current address */
    def + (register:Address.Item):Address = Address(this.address,registers ::: List(register))
}

object Address {
  def apply(address:Int, registers:Address.Item*)      = new Impl(address,registers.toList)
  def apply(address:Int, registers:List[Address.Item]) = new Impl(address,registers)

  class Impl(override val address:Int, override val registers:List[Address.Item]) extends Address

  class Item(val location:Int, val register:Register) {
    def -- (description:Description) = {
      register.description = Some(description)
    }
  }

}