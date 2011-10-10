package com.simplifide.generate.blocks.proc

import com.simplifide.generate.signal.{FixedType, OpType, SignalTrait}

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 8/17/11
 * Time: 6:15 PM
 * To change this template use File | Settings | File Templates.
 */

/** Class containing a list of registers for a processor interface */
trait RegisterMap {
  // Map of Addresses
  val addresses:Map[Int,Address]

  /** List of addresses sorted */
  def sortedAddresses:List[(Int,Address)] =
    addresses.map(x => (x._1,x._2)).toList.sortBy(x => x._1)

  /** List of All Address Items which are in this map */
  def sortedItems:List[Address.Item] = sortedAddresses.flatMap(x => x._2.registers)

  /** Create the Output Signals */
  def outputSignals:List[SignalTrait] = sortedItems.map(x => SignalTrait(x.register.name,OpType.Output,FixedType.unsigned(x.register.width,0)) )

}

object RegisterMap {
  def apply(addresses:Map[Int,Address]) = new Impl(addresses)


  class Impl(override val addresses:Map[Int,Address]) extends RegisterMap
}