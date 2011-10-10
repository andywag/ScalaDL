package com.simplifide.generate.test

import com.simplifide.generate.project2.Project
import com.simplifide.generate.hier2.Entity

/**
 * Created by IntelliJ IDEA.
 * User: Andy
 * Date: 9/26/11
 * Time: 9:20 PM
 * To change this template use File | Settings | File Templates.
 */

trait SimInterface {
  def createSimFiles(files:List[String])
  def compile(entity:Entity)
  def run(entity:Entity)
}