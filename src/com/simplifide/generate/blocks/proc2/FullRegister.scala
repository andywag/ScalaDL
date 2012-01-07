package com.simplifide.generate.blocks.proc2

import com.simplifide.generate.html.Description
import com.simplifide.generate.signal.SignalTrait


/**
 * Register and the addresses
 */

trait FullRegister {
  /** Base Register */
  val register:RegisterNew
  /** Location of Register */
  val location:FullRegister.Location
  /** Description of Register */
  val description:Description

  // Attach a Location to the register
  def at(address:Int,start:Int):FullRegister = FullRegister(this.register,new FullRegister.Location(address,start,start+this.register.signal.fixed.width))
  def at(location:FullRegister.Location):FullRegister = FullRegister(this.register,location)


  def -- (description:Description) = new FullRegister.Impl(register,location,description)
  def comment (description:Description) = new FullRegister.Impl(register,location,description)

  def createAddress(base:Int) = {

  }
}

object FullRegister {
  //def apply(register:RegisterNew,address:Int, start:Int, stop:Int):FullRegister =
  //  FullRegister(register,new FullRegister.Location(address,start,stop))


  def apply(register:RegisterNew):FullRegister =
    new FullRegister.Impl(register,FullRegister.NoLocation)
  
  def apply(register:RegisterNew,location:FullRegister.Location):FullRegister =
    new FullRegister.Impl(register,location)

  
  class Impl(override val register:RegisterNew,
             override val location:FullRegister.Location,
             override val description:Description = Description.Empty) extends FullRegister {
    
  }
  
  
  class Location (val address:Int, val start:Int, val stop:Int)
  object NoLocation extends Location(0,0,0)

}