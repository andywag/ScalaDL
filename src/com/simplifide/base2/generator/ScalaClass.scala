package com.simplifide.base2.generator

import com.simplifide.base2.generator.ScalaClass.Impl

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 3/4/12
 * Time: 12:08 PM
 * To change this template use File | Settings | File Templates.
 */

trait ScalaClass extends ScalaObject {

  val className:String
  val parent:String

  val imports:List[ScalaObject]
  val items:List[ScalaObject]

  def header = {
    this match {
      case x:ScalaClass.Object => "object"
      case _                   => "class"
    }
  }

  override def generate = {
    val head = header + " " + className.capitalize + " extends " + parent + " {\n\n"
    val body = items.map(_.generate).foldLeft("")(_+_)
    val tail = "}\n"
    head + body + tail
  }


}

object ScalaClass {

  trait Object extends ScalaClass


  def apply(className:String,
    parent:String,
    items:List[ScalaObject],
    imports:List[ScalaObject] = List()) = new Impl(className,parent,items,imports)

  class Impl(val className:String,
    val parent:String,
    val items:List[ScalaObject],
    override val imports:List[ScalaObject]) extends ScalaClass

}
