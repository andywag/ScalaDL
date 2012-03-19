package com.simplifide.generate.blocks.statemachine2

import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, SimpleSegment}
import com.simplifide.generate.signal.{Constant, ParameterTrait}

/**
 *  Class Describing a state for the state matchine
 */

trait State {
  /** Name of State */
  val name:String 
  /** Index of the State */
  val index:Int
  /** List of Possible Transitions from this state */
  val transitions:List[Transition]
  /** List of Operations which occur during this state */
  val operations:List[SimpleSegment]

  /** Copy this state */
  def copy(name:String = this.name,
    index:Int = this.index,
    transitions:List[Transition] = this.transitions,
    operations:List[SimpleSegment] = this.operations):State

  val parameter = new ParameterTrait.Hex(name,index.toHexString)

  
}

object State {
  
  def apply(name:String, index:Int) = new Impl(name,index,List(),List())
  
  class Impl(override val name:String,
    override val index:Int,
    override val transitions:List[Transition],
    override val operations:List[SimpleSegment]) extends State with SimpleSegment {

    def createCode(implicit writer:CodeWriter):SegmentReturn =
      Constant(index).createCode


    def copy(name:String = this.name,
             index:Int = this.index,
             transitions:List[Transition] = this.transitions,
             operations:List[SimpleSegment] = this.operations):State
      = new Impl(name,index,transitions,operations)

  }
}