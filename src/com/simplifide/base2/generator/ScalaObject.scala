package com.simplifide.base2.generator

/**
 * Object which is used in the generation of a scala file
 */

trait ScalaObject {
  def generate:String
  val imports:List[ScalaObject] = List()

}
