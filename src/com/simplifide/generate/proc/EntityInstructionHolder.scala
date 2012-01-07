package com.simplifide.generate.proc

import com.simplifide.generate.project.Entity
import com.simplifide.generate.blocks.basic.flop.ClockControl

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 11/30/11
 * Time: 1:52 PM
 * To change this template use File | Settings | File Templates.
 */

class EntityInstructionHolder(val child:Entity)(implicit clk:ClockControl) extends Entity.Branch(child.name + "_top",child.name + "_top") {

}