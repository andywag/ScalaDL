package com.simplifide.generate.test

import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.project.Entity

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 9/22/11
 * Time: 1:47 PM
 * To change this template use File | Settings | File Templates.
 */

class TestEntity(val testModule:TestModule)(implicit clk:ClockControl) extends Entity.Root(testModule.name,testModule.name){

  override lazy val entities = List(testModule.dut)
  override def createModule = testModule.createModule

}