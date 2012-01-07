package com.simplifide.generate.blocks.proc2.parser

import com.simplifide.generate.html.Description
import com.simplifide.generate.blocks.proc2.{RegisterGroup, RegisterNew, FullRegister}
import com.simplifide.generate.blocks.proc2.parser.RegisterModel.AddressSection

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 1/4/12
 * Time: 2:20 PM
 * To change this template use File | Settings | File Templates.
 */

class RegisterModel {

}

object RegisterModel {


  class Section(val registers:List[FullRegister], val next:FullRegister) extends RegisterParser.Builder {

    /** Default Copy Operation */
    def copy(registers:List[FullRegister] = this.registers, next:FullRegister = this.next) =
      new Section(registers,next)

    /** Create an address which only defines the starting location */
    def at(start:Int) = {
      val location = new FullRegister.Location(0,start,start+next.register.signal.fixed.width)
      this.copy(next = next.at(location))
    }
    /** Attach a location to the current register */
    def at(address:Int,start:Int) = {
      val location = new FullRegister.Location(address,start,start+next.register.signal.fixed.width)
      this.copy(next = next.at(location))
    }
    /** Attach a comment to the current register */
    def comment (description:Description) = {
      this.copy(next = new FullRegister.Impl(next.register,next.location,description))
    }



    override def sectionOpen(register:RegisterNew)  = this.copy(registers ::: List(next), FullRegister(register))

    def createGroup(base:Int) = RegisterGroup(base,registers ::: List(next))

  }
 
  
  object Section {
    def apply(next:FullRegister) = new Section(List(),next)
    def apply(registers:List[FullRegister],next:FullRegister) = new Section(registers,next)
  }
  
  class AddressSection(val base:Int, registers:List[FullRegister],next:FullRegister) extends Section(registers,next) {
    //override def copy(registers:List[FullRegister] = this.registers, next:FullRegister = this.next) =
    //  new AddressSection(registers,next)

    /** Attach a location to the current register */

  }
  


  class Group {
    
  }
  

  
  class Address {
    
  }
  


  
}