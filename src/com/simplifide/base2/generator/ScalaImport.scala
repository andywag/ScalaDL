package com.simplifide.base2.generator

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 3/5/12
 * Time: 10:44 AM
 * To change this template use File | Settings | File Templates.
 */

class ScalaImport(val className:String) extends ScalaObject {

  def generate = "import " + className + "\n"
}