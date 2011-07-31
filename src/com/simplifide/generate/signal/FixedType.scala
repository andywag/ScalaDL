package com.simplifide.generate.signal

import com.simplifide.generate.parser.model.Model


/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/** Fixed type trait */
trait FixedType extends Model.Fixed{
  /** Signing of the value either signed or unsigned */
  val signed:Signing
  /** Width of the Signal */
  //override val width:Int
  /** Fraction Point */
  //override val fraction:Int

  /** @deprecated -- use integer*/
  val integerWidth:Int = width - fraction

  def increment(width1:Int, frac1:Int = 0):FixedType = FixedType(this.signed,this.width + width1, this.fraction + frac1)

  val integer:Int = width - fraction

  def getDescription:String = return "<" + width + "," + fraction + ">"

  def equals(typ:FixedType):Boolean = {
    if (typ.width == this.width && typ.fraction == this.fraction)
    	return true
    else 
    	return false
  }

  def plus(fix:FixedType):FixedType = {
    return new FixedType.Main(signed,this.width + fix.width,this.fraction + fix.fraction)
  }

  /** Multiply 2 fixed point numbers together */
  def * (fix:FixedType) = FixedType(this.signed,this.width + this.width, this.fraction + fix.fraction)
  /** Create the resulted width by adding two fixed types together */
  def + (fix:FixedType):FixedType = FixedType(signed,this.width + fix.width,this.fraction + fix.fraction)

  def == (fix:FixedType):Boolean = equals(fix)

  /** Create the total width which would occur by adding 2 fixed types together */
  def union(fixed:FixedType*):FixedType = {
    val integer     = fixed.map(_.integer).reduceLeft(math.max(_,_)) // math.max(this.integer,fixed.integer)
    val fraction    = fixed.map(_.fraction).reduceLeft(math.max(_,_)) //math.max(this.fraction,fixed.fraction)
    FixedType(signed,integer+fraction,fraction)
  }

  /** Returns the Wire Declaration Associated with this fixed type */
  def getWireDeclaration:String = {
    val builder = new StringBuilder()
    builder.append("wire ")
    if (signed.isSigned) builder.append("signed ")
    if (width > 0) {
      builder.append("[")
      builder.append(width-1)
      builder.append(":0] ")
    }
    return builder.toString
  }

  def getOrElse(fixed:FixedType):FixedType = this

}

object FixedType {
  def apply(signed:Signing, width:Int,frac:Int) = new FixedType.Main(signed,width,frac)

  def signed(fixed:FixedType):FixedType           = signed(fixed.width,fixed.fraction)
  def signed(width:Int, fraction:Int):FixedType   = new FixedType.Main(Signing.Signed,width,fraction)
  def unsigned(width:Int, fraction:Int):FixedType = new FixedType.Main(Signing.UnSigned,width,fraction)

  class Main(val signed:Signing, val width:Int,val fraction:Int) extends FixedType
  object Simple extends Main(Signing.UnSigned,1,0)
  object None   extends Main(Signing.UnSigned,1,0) {
    override def getOrElse(fixed:FixedType):FixedType = fixed
  }


}
