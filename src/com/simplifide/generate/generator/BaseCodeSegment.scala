package com.simplifide.generate.generator

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: Aug 24, 2010
 * Time: 11:28:35 PM
 * To change this template use File | Settings | File Templates.
 */

import com.simplifide.generate.signal.FixedType



class BaseCodeSegment extends com.simplifide.generate.generator.SimpleSegment{

  /** Get the Number of elements that this segment has */
  def getNumber:Int = 0

  /** Get the signal type associated with this segment */
  def getFixedType:FixedType = null

  /** Get the part select for this part */
  def getSelect(fixed:FixedType):BaseCodeSegment = this

  /** Creates the code for this segment. Called with one of the writers */
  //def createCode(creator:(BaseCodeSegment)=>String):String = ""

  /** Creators for the different code types */
  //def createVerilog():String =  createCode(BaseCodeSegment.verilogWriter(_))
  //def createVhdl():String    =  createCode(BaseCodeSegment.vhdlWriter(_))
  //def createC():String       =  ""

  //def createCode:String = return createCode(null)



  /*def createCode(creator:(BaseCodeSegment,CurrentContext) => SegmentReturn, context:CurrentContext):SegmentReturn = {
    return creator(this,context)
  }*/

  override def createVerilogCode(writer:CodeWriter):SegmentReturn     = createCode(writer)
  override def createVhdlCode(writer:CodeWriter):SegmentReturn        = createCode(writer)
  override def createFloatCode(writer:CodeWriter):SegmentReturn       = createCode(writer)
  override def createFixedCode(writer:CodeWriter):SegmentReturn       = createCode(writer)
  override def createHeaderCode(writer:CodeWriter):SegmentReturn      = createCode(writer)




  def createCode(writer:CodeWriter):SegmentReturn = SegmentReturn.segment("")


  def createList(segments:List[BaseCodeSegment],writer:CodeWriter):SegmentReturn = {
      val ret:List[SegmentReturn] = segments.map(x => writer.createCode(x))
      return SegmentReturn.combineReturns(ret, List())
  }

  def createList(segments:List[BaseCodeSegment],writer:CodeWriter,errors:List[InterfaceError]):SegmentReturn = {
      val ret:List[SegmentReturn] = segments.map(x => writer.createCode(x))
      return SegmentReturn.combineReturns(ret, List())
  }

  def createVhdlList(segments:List[BaseCodeSegment], writer:CodeWriter):SegmentReturn = {
    createList(segments,writer)
  }

  def createVerilogList(segments:List[BaseCodeSegment], writer:CodeWriter):SegmentReturn = {
    createList(segments,writer)
  }



}