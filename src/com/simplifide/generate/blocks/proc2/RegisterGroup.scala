package com.simplifide.generate.blocks.proc2

/**
 * Group of Registers
 */

trait RegisterGroup {
  /** Base Address for Group of Registers */
  val base:Int
  /** List of Addresses contained in this map*/
  val addresses:List[AddressNew]
  /** List of registers contained in this group */
  //val registers:List[FullRegister]
  /** List of Addresses contained in this register group */

  /*val addresses:List[AddressNew] = {
    val groups = registers.groupBy(x => x.location.address) // Group the registers into addresses
    groups.map(x => AddressNew(base + x._1,x._2)).toList
  }
  */

}

object RegisterGroup {
  def apply(base:Int, registers:List[FullRegister]) = {
    val groups = registers.groupBy(x => x.location.address) // Group the registers into addresses
    val addresses = groups.map(x => AddressNew(base + x._1,x._2)).toList
    new RegisterGroup.Impl(base,addresses)
  }

  
  class Impl(override val base:Int, override val addresses:List[AddressNew]) extends RegisterGroup
  
}