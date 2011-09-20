package com.simplifide.generate.signal

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 8/26/11
 * Time: 10:13 PM
 * To change this template use File | Settings | File Templates.
 */

/** Value used to model a parameter for the module. Uses mutable methods but  */
trait Parameter[T] {
  val name:String
  val default:T
  var value:Option[T] = None

  def get:T = value.getOrElse(default)

  def set(value:T) {
    this.value = Some(value)
  }

}

object Parameter {

  def apply[T](name:String, default:T) = new Imp[T](name,default)

  class Imp[T](override val name:String, override val default:T) extends Parameter[T]

}