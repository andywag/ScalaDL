package com.simplifide.generate.generator

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import scala.collection.mutable.ListBuffer
import com.simplifide.generate.signal.SignalTrait
import com.simplifide.generate.util.StringOps

class SegmentReturn(val code:String,
                    val errors:List[InterfaceError],
                    val extra:List[SimpleSegment] = List[SimpleSegment](),
                    val internal:List[SignalTrait] = List[SignalTrait]()) {

  /** deprecated : use extra instead*/
  //val extraStatements:ListBuffer[Statement] = new ListBuffer[Statement]()
  
  override def toString():String = code

  /** Combine the segment returns */
  def + (ret:SegmentReturn) = new SegmentReturn(this.code + ret.code,
        this.errors ::: ret.errors,
        this.extra ::: ret.extra,
        this.internal ::: ret.internal)

  def ++ (ret:SegmentReturn) = new SegmentReturn(this.code + StringOps.indentLines(ret.code,1),
        this.errors ::: ret.errors,
        this.extra ::: ret.extra,
        this.internal ::: ret.internal)

  /** Add a segment return with a a String */
  def + (ret:String) = new SegmentReturn(this.code + ret,this.errors,this.extra, this.internal)


  def combine(in:SegmentReturn ):SegmentReturn = {
    val ret = new SegmentReturn(this.code + in.code, this.errors ::: in.errors, this.extra ::: in.extra)
    return ret
  }

}

object SegmentReturn {

  implicit def string2SegmentReturn(str:String):SegmentReturn = SegmentReturn.segment(str)

  def segment(code:String)        = new SegmentReturn(code,List())
  def segment(error:InterfaceError) = new SegmentReturn("",List(error))
  def error(error:String)         = segment(new InterfaceError.Error(0,error))


  def combineBufferReturns(writer:CodeWriter,segs:ListBuffer[SimpleSegment]):SegmentReturn = {
    val returns = segs.map(x => writer.createCode(x)).toList
    return combineReturns(returns)
  }

  def combineListReturns(writer:CodeWriter,segs:List[SimpleSegment]):SegmentReturn = {
    val returns = segs.map(x => writer.createCode(x)).toList
    return combineReturns(returns)
  }

  def combineReturns(writer:CodeWriter,segs:ListBuffer[SimpleSegment]):SegmentReturn = {
    val returns = segs.map(x => writer.createCode(x)).toList
    return combineReturns(returns)
  }
  def combineReturns(segs:List[SegmentReturn]):SegmentReturn = combineReturns(segs,List())

  def combineFinalReturns(writer:CodeWriter,segments:List[SimpleSegment],internals:List[SignalTrait]):SegmentReturn  =
    combineReturns(segments.map(writer.createCode(_)),List(),internals)

  def combineReturns(segs:List[SegmentReturn], extra:List[InterfaceError], internals:List[SignalTrait] = List()):SegmentReturn = {
     val builder = new StringBuilder()
     val buffer  = new ListBuffer[InterfaceError]()
     buffer.appendAll(extra)
     for (seg <- segs) {
       builder.append(seg.code)
       buffer.appendAll(seg.errors)
     }
    return new SegmentReturn(builder.toString,buffer.toList, segs.flatMap(x => x.extra),segs.flatMap(x => x.internal) ::: internals)
  }



  class NotDefined(override val code:String, cla:String) extends SegmentReturn(code,List(new InterfaceError.Error(0,cla + " Not Defined")))
}





 