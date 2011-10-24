package com.simplifide.generate.blocks.proc

import com.simplifide.generate.blocks.basic.condition.{ NewCaseStatement}
import com.simplifide.generate.parser.block.Statement
import com.simplifide.generate.blocks.basic.SimpleStatement
import com.simplifide.generate.signal.{OpType, Constant}
import com.simplifide.generate.blocks.basic.operator.Select
import com.simplifide.generate.generator.{BasicSegments, CodeWriter, SimpleSegment}
import com.simplifide.generate.blocks.basic.state.AlwaysProcess
import com.simplifide.generate.blocks.basic.flop.{SimpleFlop}

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 8/17/11
 * Time: 6:23 PM
 * To change this template use File | Settings | File Templates.
 */


/** Class which decodes the addresses for the processor interface */
abstract class AddressDecoder(val registerMap:RegisterMap, val bus:ProcessorBus) extends SimpleSegment{

}

object AddressDecoder {

  class Write(registerMap:RegisterMap,bus:ProcessorBus) extends AddressDecoder(registerMap,bus) {

    private def createAddressStatements(address:Address):NewCaseStatement.Item = {
      // Create the Write Address Select Lines
      def createSlice(item:Address.Item) = Select(bus.writeData,item.location + item.register.width-1,item.location)
      // Create the Assignment Registers
      def createReg(x:Address.Item) = new SimpleStatement.Reg(x.register.signal(OpType.Register),createSlice(x))
      // Individual Statements
      val states = address.registers.map(x => createReg(x))
       NewCaseStatement.Item(Constant(address.address,bus.writeAddress.fixed.width),BasicSegments.ListSurround(states))
    }
    override def createCode(writer:CodeWriter) = {
      val cas = NewCaseStatement(bus.writeAddress,registerMap.sortedAddresses.map(x => createAddressStatements(x._2)))
      val res = registerMap.sortedItems.map(x => new SimpleStatement.Reg(x.register.signal(OpType.Register),Constant(0,x.register.signal(OpType.Register).fixed.width)))
      val flop = new SimpleFlop(None,bus.clk.createEnable(bus.writeValid),
        BasicSegments.ListSurround(res),
        cas)
      writer.createCode(flop)
    }

  }


  class Read(registerMap:RegisterMap,bus:ProcessorBus) extends AddressDecoder(registerMap,bus) {
     private def createAddressStatements(address:Address):NewCaseStatement.Item = {
      // Create the Write Address Select Lines
      def createSlice(item:Address.Item) = Select(bus.readData,item.location + item.register.width-1,item.location)
      // Create the Assignment Registers
      def createReg(x:Address.Item) = new SimpleStatement.Reg(createSlice(x),x.register.signal(OpType.Register))
      // Individual Statements
      val states = address.registers.map(x => createReg(x))
       NewCaseStatement.Item(Constant(address.address,bus.writeAddress.fixed.width),BasicSegments.ListSurround(states))
    }
    override def createCode(writer:CodeWriter) = {
      val cas = NewCaseStatement(bus.readAddress,registerMap.sortedAddresses.map(x => createAddressStatements(x._2)))
      val flop = new SimpleFlop(None,bus.clk,
        new SimpleStatement.Reg(bus.readData,Constant(0,bus.readData.fixed.width)),
        cas)
      writer.createCode(flop)
    }
  }

}