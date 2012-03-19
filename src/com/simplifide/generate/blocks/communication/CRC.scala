package com.simplifide.generate.blocks.communication

import com.simplifide.generate.generator.{SimpleSegment, ComplexSegment}
import com.simplifide.generate.blocks.basic.operator.BinaryOperator
import com.simplifide.generate.signal.Constant._
import com.simplifide.generate.signal.{Constant, FixedType, SignalTrait}
import com.simplifide.generate.parser.model.Expression
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.blocks.statemachine2.parser.StateMachineParser
import com.simplifide.generate.blocks.statemachine2.StateMachine

/**
 *  Block which handles CRC functionallity
 */

class CRC(val dataIn:SignalTrait,
  val valid:SignalTrait,
  val signalLength:SignalTrait,
  val fail:SignalTrait,
  val poly:List[Int],
  val length:Int,
  val init:List[Int])(implicit clk:ClockControl) extends ComplexSegment {

  val state = new CRC.CrcState(this)

  def createBody {
    this.assign(state)


    val crcInput = signal("crc_result_input")
    val compare  = signal("compare")
    val compareR  = signal("compareR",REG)
    val done      = signal("done")

    val result   = signal("data_result",WIRE,U(length,0))
    val resultR  = signal("data_resultR",REG,U(length,0))


    /- ("Reset Generation")
    crcInput := (state.current == state.MATCH) ? 0 :: dataIn

    resultR := result $at(clk.createEnable(valid))
    for (x <- 0 until length) {
      if (x == 0)                result(x) := crcInput
      else if (poly.contains(x)) result(x) := resultR(x-1) ^ crcInput
      else                       result(x) := resultR(x-1)
    }

    
    compare  := (resultR(length-1) != dataIn) & (state.current == state.MATCH)
    compareR := (compareR | compare) $at (clk)
    done     := state.current == state.DONE
    fail     := compareR $at clk.createEnable(done)
  }



}

object CRC {

  class CrcState(crc:CRC)(implicit clk:ClockControl) extends ComplexSegment with StateMachineParser {
    val current = signal("current",REG,unsigned(3,0))
    val count   = signal("counter",REG,unsigned(13,0))

    val IDLE  = state("IDLE",0x0)
    val DATA  = state("DATA",0x1)
    val MATCH = state("MATCH",0x2)
    val DONE   = state("DONE",0x4)

    transition (
      IDLE  to DATA  when (crc.valid),
      DATA  to MATCH when (count == crc.signalLength - crc.length-1),
      MATCH to DONE  when (count == (crc.signalLength-1)) ,
      DONE  to IDLE  when (1)
    )

    def createBody = {
      /- ("State Machine Definition")
      this.assign(new StateMachine(this.finalStates,current))
      /- ("Counter Control")
      count := current $match (
        $cases(DATA)  $then count + 1
        $cases(MATCH) $then count + 1
        $cases(DONE)  $then 0
      ) $at (clk)
    }

  }




  
}