package com.simplifide.generate.test

import com.simplifide.generate.project.Project
import com.simplifide.generate.blocks.basic.flop.ClockControl

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 9/22/11
 * Time: 11:35 AM
 * To change this template use File | Settings | File Templates.
 */

trait Test {

  /** Test Entity */
  val testEntity:TestEntity


  def createTest(project:Project) = {
    testEntity.writeModule(project.projectStructure.test)
  }

}

object Test {
  def apply(module:TestModule)(implicit clk:ClockControl) = new Impl(new TestEntity(module))

  class Impl(override val testEntity:TestEntity) extends Test

}