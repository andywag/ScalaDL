package com.simplifide.generate.hier2

import com.simplifide.generate.signal.SignalTrait
import com.simplifide.generate.blocks.basic.flop.ClockControl

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 9/8/11
 * Time: 3:53 PM
 * To change this template use File | Settings | File Templates.
 */

/** Class which is used when the hierarchy is being created */
class ExpandedEntity (val entity:Entity,
                      override val signals:List[SignalTrait],
                      override val entities:List[Entity])(implicit clk:ClockControl) extends Entity.Branch(entity.name,entity.connectionName,entity.converter) {

  override def createModule = entity.createModule


}

object ExpandedEntity {

}