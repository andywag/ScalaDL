package com.simplifide.generate.project

import com.simplifide.generate.signal.SignalTrait
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.generator.SimpleSegment

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 9/8/11
 * Time: 3:53 PM
 * To change this template use File | Settings | File Templates.
 */

/** Class which is created when an entity is having  */
class ExpandedEntity (val entity:Entity,
  override val internalSignals:List[SignalTrait],
  override val entitySignals:List[SignalTrait],
  override val instances:List[EntityInstance[Entity]],
  override val internalStatements:List[SimpleSegment] = List())(implicit clk:ClockControl) extends Entity.Branch(entity.name,entity.connectionName,entity.converter) {

  override def createModule:ModuleProvider = {
    val states = this.statements.flatMap(_.split).toList.map(_.asInstanceOf[SimpleSegment]) :::
      this.internalStatements.flatMap(_.split).toList.map(_.asInstanceOf[SimpleSegment])
    ModuleProvider(name,null,this.entitySignals ::: internalSignals,states,this.instances,List())
  }

}

object ExpandedEntity {

}