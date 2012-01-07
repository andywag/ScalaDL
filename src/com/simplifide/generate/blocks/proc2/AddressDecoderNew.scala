package com.simplifide.generate.blocks.proc2

import com.simplifide.generate.blocks.proc.{ProcessorBus, RegisterMap}
import com.simplifide.generate.generator.ComplexSegment
import com.simplifide.generate.blocks.basic.flop.ClockControl

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 1/3/12
 * Time: 1:36 PM
 * To change this template use File | Settings | File Templates.
 */

trait AddressDecoderNew {
  val registerMap:RegisterMapNew
  val bus:ProcessorBus

}

object AddressDecoderNew {
  
  class Read(override val registerMap:RegisterMapNew, override val bus:ProcessorBus)  extends ComplexSegment with AddressDecoderNew {

    private def createAddress(address:AddressNew) = {
      def individual(register:FullRegister) = bus.readData((register.location.stop,register.location.start)) ::= register.register.signal
      val results = address.registers.filter(_.register.isRead)
      $cases(address.address) $then (results.map(individual(_)))
    }

    def createBody {
      $always_body(
        bus.readAddress $match (
          registerMap.addresses.map(x => createAddress(x))
        )
      )
    }
  }


  class Write(override val registerMap:RegisterMapNew, override val bus:ProcessorBus)(implicit clk:ClockControl) extends ComplexSegment with AddressDecoderNew {

    private def createAddress(address:AddressNew) = {
      def individual(register:FullRegister) = register.register.signal ::= bus.writeData((register.location.stop,register.location.start))
      $cases(address.address) $then (address.registers.filter(_.register.isWrite).map(individual(_)))
    }
    
    def createBody {
      $always_clk(clk) (
        bus.writeAddress $match (
          registerMap.addresses.map(x => createAddress(x))
        )
      )

    }
  }

  
}