package com.simplifide.generate.signal

import com.simplifide.generate.parser.model.Model
import com.simplifide.generate.signal.FixedType.Signing



/**
 * Class describing the fixed type properties of a signal
 **/
trait FixedType extends Model.Fixed{
  /** Signing of the value either signed or unsigned */
  val signed:Signing
  /** Number of Integer bits */
  val integer:Int = width - fraction
  /** Returns the description for this type */
  def getDescription:String = return "<" + width + "," + fraction + ">"

  private def equals(typ:FixedType):Boolean =
    if (typ.width == this.width && typ.fraction == this.fraction) return true
    else return false


  def plus(fix:FixedType):FixedType = {
    return new FixedType.Main(signed,this.width + fix.width,this.fraction + fix.fraction)
  }

  /** Result of multiplying 2 fixed types together */
  def * (fix:FixedType) = FixedType(this.signed,this.width + fix.width, this.fraction + fix.fraction)
  /** Result of adding 2 fixed types together */
  def + (fix:FixedType):FixedType = FixedType(signed,this.width + fix.width,this.fraction + fix.fraction)

  def == (fix:FixedType):Boolean = equals(fix)

  /** Create the total width which would occur by adding 2 fixed types together */
  def union(fixed:FixedType*):FixedType = {
    val integer     = math.max(this.integer,fixed.map(_.integer).reduceLeft(math.max(_,_))) // math.max(this.integer,fixed.integer)
    val fraction    = math.max(this.fraction,fixed.map(_.fraction).reduceLeft(math.max(_,_))) //math.max(this.fraction,fixed.fraction)
    FixedType(signed,integer+fraction,fraction)
  }



  def getOrElse(fixed:FixedType):FixedType = this

}

/**
 * Factory methods for creating a fixed point type
 **/
object FixedType {
  def apply(signed:Signing, width:Int,frac:Int) = new FixedType.Main(signed,width,frac)
  /** Creation of Signed type */
  def signed(fixed:FixedType):FixedType           = signed(fixed.width,fixed.fraction)
  /** Creation of signed type with width width and fraction */
  def signed(width:Int, fraction:Int):FixedType   = new FixedType.Main(Signing.Signed,width,fraction)
  /** Creation of unsigned type with width width and fraction */
  def unsigned(width:Int, fraction:Int):FixedType = new FixedType.Main(Signing.UnSigned,width,fraction)
  /** Implementation of Fixed Type */
  class Main(val signed:Signing, val width:Int,val fraction:Int) extends FixedType
  object Simple extends Main(Signing.UnSigned,1,0)  {
    override def getOrElse(fixed:FixedType):FixedType = fixed
  }


  class Signing {
    val isSigned:Boolean = false;
    val isControl:Boolean = false;

  }

  object Signing {
    object Signed extends Signing {
      override val isSigned:Boolean = true;
    }

  object UnSigned extends Signing

  object Control extends Signing {
    override val isControl:Boolean = true
  }

}


}
