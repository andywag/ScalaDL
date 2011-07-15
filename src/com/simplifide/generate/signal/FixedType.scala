package com.simplifide.generate.signal


/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/** Fixed type trait */
trait FixedType {
  /** Signing of the value either signed or unsigned */
  val signed:Signing
  /** Width of the Signal */
  val width:Int
  /** Fraction Point */
  val frac:Int

  /** @deprecated -- use integer*/
  val integerWidth:Int = width - frac

  def increment(width1:Int, frac1:Int = 0):FixedType = FixedType(this.signed,this.width + width1, this.frac + frac1)

  val integer:Int = width - frac

  def getDescription:String = return "<" + width + "," + frac + ">"

  def equals(typ:FixedType):Boolean = {
    if (typ.width == this.width && typ.frac == this.frac) 
    	return true
    else 
    	return false
  }

  def plus(fix:FixedType):FixedType = {
    return new FixedType.Main(signed,this.width + fix.width,this.frac + fix.frac)
  }

  /** Create the resulted width by adding two fixed types together */
  def + (fix:FixedType):FixedType = {
    return new FixedType.Main(signed,this.width + fix.width,this.frac + fix.frac)
  }
  /** Create the total width which would occur by adding 2 fixed types together */
  def union(fixed:FixedType):FixedType = {
    val integer     = math.max(this.integer,fixed.integer)
    val fraction    = math.max(this.frac,fixed.frac)
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
}

object FixedType {
  def apply(signed:Signing, width:Int,frac:Int) = new FixedType.Main(signed,width,frac)

  def signed(fixed:FixedType):FixedType           = signed(fixed.width,fixed.frac)
  def signed(width:Int, fraction:Int):FixedType   = new FixedType.Main(Signing.Signed,width,fraction)
  def unsigned(width:Int, fraction:Int):FixedType = new FixedType.Main(Signing.UnSigned,width,fraction)

  class Main(val signed:Signing, val width:Int,val frac:Int) extends FixedType
  object Simple extends Main(Signing.UnSigned,1,0)


}
