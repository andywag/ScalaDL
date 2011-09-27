package com.simplifide.generate.signal

import com.simplifide.generate.generator.SimpleSegment


/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 6/8/11
 * Time: 9:18 PM
 * To change this template use File | Settings | File Templates.
 */

trait ArrayTrait[T <: SignalTrait] extends SignalTrait {

  val length:Int;
  val prototype:T

  override def apply(index:Int):T = this.slice(index)

  override val name:String = prototype.name
  override val opType:OpType = prototype.opType
  override val fixed:FixedType = prototype.fixed

  override val numberOfChildren:Int = length


  override  def changeTestType:SignalTrait = ArrayTrait(prototype.changeTestType,this.length)
  /** Changes the type of the signal. Mainly used for Input Output Changes during connections */
  override def changeType(typ:OpType):SignalTrait = ArrayTrait(prototype.changeType(typ),this.length)

  def newObject(length:Int,prototype:T):ArrayTrait[T]

  override def children:List[SignalTrait] = {
    List.tabulate(length)(i => this.prototype.createSlice(i))
  }
  override def allChildren:List[SimpleSegment] = {
    val children = this.children;
    children.flatMap(x => x.allChildren)
  }


  // TODO This case needs to be handled. This is for an array of an array
  //override def createSlice(index:Int):SignalTrait = newObject(length,prototype)

  /** Slice is created by getting the slice variable from the prototype which creates the new variable */
  override def slice(index:Int):T = prototype.createSlice(index).asInstanceOf[T]


}

object ArrayTrait {
  def apply[T <: SignalTrait](prototype:T,length:Int):ArrayTrait[T]       = new Array(length,prototype)

  def newArray[T <: SignalTrait](length:Int,prototype:T):ArrayTrait[T]       = new Array(length,prototype)



  class Array[T <: SignalTrait](val length:Int,val prototype:T) extends ArrayTrait[T] {
      def newObject(length:Int,prototype:T):ArrayTrait[T] = new Array[T](length,prototype)
      override def newSignal(nam:String,optype:OpType,fix:FixedType):SignalTrait = this
  }
}