package com.simplifide.generate.signal


/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 6/8/11
 * Time: 9:18 PM
 * To change this template use File | Settings | File Templates.
 */

trait ArrayTrait2[T <: SignalTrait] extends SignalTrait {

  val length:Int;
  val prototype:T

  override val name:String = prototype.name
  override val opType:OpType = prototype.opType
  override val fixed:FixedType = prototype.fixed

  override val numberOfChildren:Int = length

  def newObject(length:Int,prototype:T):ArrayTrait2[T]

  override def getChildren:List[SignalTrait] = {
    List.tabulate(length)(i => this.prototype.createSlice(i)).flatMap(_.getChildren)
  }

  // TODO This case needs to be handled. This is for an array of an array
  //override def createSlice(index:Int):SignalTrait = newObject(length,prototype)

  /** Slice is created by getting the slice variable from the prototype which creates the new variable */
  override def slice(index:Int):T = prototype.createSlice(index).asInstanceOf[T]


}

object ArrayTrait2 {
  def apply[T <: SignalTrait](prototype:T,length:Int):ArrayTrait2[T]       = new Array(length,prototype)

  def newArray[T <: SignalTrait](length:Int,prototype:T):ArrayTrait2[T]       = new Array(length,prototype)



  class Array[T <: SignalTrait](val length:Int,val prototype:T) extends ArrayTrait2[T] {
      def newObject(length:Int,prototype:T):ArrayTrait2[T] = new Array[T](length,prototype)
      override def newSignal(nam:String,optype:OpType,fix:FixedType):SignalTrait = this

  }
}