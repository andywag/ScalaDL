package com.simplifide.generate.blocks.basic.memory

import com.simplifide.generate.project.EntityInstance._
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.project.{EntityInstance, Entity, ModuleProvider}
import com.simplifide.generate.blocks.basic.SimpleStatement
import com.simplifide.generate.proc.Controls._
import com.simplifide.generate.generator.{ComplexSegment, SimpleSegment}

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 11/9/11
 * Time: 2:40 PM
 * To change this template use File | Settings | File Templates.
 */

class MemoryEntity(val memory:Memory with SimpleSegment)(implicit clk:ClockControl) extends Entity.Leaf(memory.model.name,memory.model.name) {

  def apply(address:Int) = memory(address)
  def apply(address:Int,index:Int) = memory(address,index)

  override val entitySignals =  clk.allSignals(INPUT) ::: memory.writeBuses ::: memory.readBuses
  override lazy val controls = memory.controls

  this.assign(memory)


}

object MemoryEntity {

}