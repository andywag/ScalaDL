package com.simplifide.generate.blocks.proc

import com.simplifide.generate.signal.{FixedType, OpType, SignalTrait}
import com.simplifide.generate.html.Description
import com.simplifide.generate.parser.RegisterMapHolder

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 8/16/11
 * Time: 3:26 PM
 * To change this template use File | Settings | File Templates.
 */

trait Register {
  /** Variable Name of Register */
  val name:String
  /** Width of Register */
  val width:Int
  /** Default Value of Register */
  val default:Long
  /** Description of Register -- Mutable to allow description to be added bit of a kludge */
  var description:Option[Description]
  /** Type of Register*/
  val registerType:Int

  val typeString = registerType match {
    case 0 => "N"
    case 1 => "R"
    case 2 => "W"
    case 3 => "RW"
  }

  def -- (description:Description):Register =
    Register(name,width,registerType,description,default)



  def signal(optype:OpType):SignalTrait = SignalTrait(name,optype,FixedType.unsigned(width,0))

}

object Register {
  val NONE       = 0
  val READ       = 1
  val WRITE      = 2
  val READ_WRITE = 3
  /** Register Creation Function */
  def apply(name:String,width:Int) =
    new Impl(name,width,READ_WRITE,0,None)
  def apply(name:String,width:Int,typ:Int) =
    new Impl(name,width,typ,0,None)
  def apply(name:String,width:Int,typ:Int ,description:Description) =
    new Impl(name,width,typ,0,Some(description))
  def apply(name:String,width:Int,typ:Int ,description:Description,default:Long) =
    new Impl(name,width,typ,default,Some(description))

  def R(width:Int) = new Reserved(width,0)




  class Impl(override val name:String,
             override val width:Int,
             override val registerType:Int = READ_WRITE,
             override val default:Long = 0,
             override var description:Option[Description] = None) extends Register

  class Reserved(override val width:Int, override val default:Long = 0) extends Impl("R",width,NONE,default)

}